{-# LANGUAGE OverloadedStrings #-}

module Compile (
    parseAllIO,
    resolveAllIO,
    inferTypeIO,
    kNormalizeIO,
    flattenExprIO,
    optimIO,
    extractGlobalsIO,
    getFunctionsIO,
    toInstructionsIO,
    optimInstIO,
    transformCodeBlockIO,
    livenessIO,
    assignRegisterIO,
) where

import BackEnd.BackendEnv (
    BackendConfig (regConfig),
    BackendEnv (regContext),
    RegConfig (regLimit),
    RegContext (generatedReg, usedRegLen),
    liftB,
 )
import BackEnd.FunctionCall (saveRegisters)
import BackEnd.Liveness (LivenessLoc (livenessLoc), liveness)
import BackEnd.Lowering (toInstructions)
import BackEnd.Optim (runBackEndOptim)
import BackEnd.Optim.Common (BackEndOptimContext (BackEndOptimContext), BackEndOptimStateT)
import BackEnd.RegisterAlloc (assignRegister)
import BackEnd.Spill (spillF, spillI)
import BackEnd.Transform (transformCodeBlock)
import CommandLine (
    BackendIdentStateIO,
    CompilerConfig (cActivatedBackEndOptim, cActivatedOptim, cEmitOptim, cInliningSize, cMaxInlining, cRecInliningSize),
    ConfigIO,
    IdentEnvIO,
 )
import Control.Exception (IOException, try)
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (StateT (runStateT), evalStateT, gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO
import Display (display)
import Error (CompilerError (OtherError, ParserError))
import FrontEnd.Flatten (flattenExpr)
import FrontEnd.KNorm (kNormalize)
import FrontEnd.NameRes (resolveNames)
import FrontEnd.Parser (parseExpr, parsePartialExpr)
import FrontEnd.TypeInferrer (inferType)
import IR (
    CodeBlock,
    IntermediateCodeBlock (getICBInst, getICBLabel),
    RegID,
    exitBlock,
    substIState,
 )
import Log (LogLevel (..), printLog, printTextLog)
import MiddleEnd.Analysis.Constant (registerConstants)
import MiddleEnd.Analysis.Identifier (IdentEnvT, loadTypeEnv)
import MiddleEnd.Closure (getFunctions)
import MiddleEnd.Desugar (expandArrayCreate)
import MiddleEnd.Globals (GlobalTable, defaultGlobalTable, extractGlobals, reportGlobals)
import MiddleEnd.Optim (runOptim)
import MiddleEnd.Optim.Common (
    OptimContext (OptimContext),
    OptimStateT,
 )
import MiddleEnd.Validator (validate)
import Path (changeExt)
import Registers (
    RegType (RFloat, RInt),
    VariantItem (VariantItem, unwrap),
    savedReg,
    updateVariant,
    (#!!),
    (#$),
 )
import Syntax (
    Function,
    KExpr,
    Loc,
    ParsedExpr,
    ResolvedExpr,
    TypedExpr,
 )
import Text.Megaparsec (MonadParsec (eof), parse)

readFileIO :: FilePath -> ConfigIO ByteString
readFileIO path = do
    content <- liftIO (try (B.readFile path) :: IO (Either IOException ByteString))
    case content of
        Left _ -> throwError $ OtherError $ "Failed to read " <> pack path
        Right content' -> pure content'

parseAllIO :: [FilePath] -> ConfigIO ParsedExpr
parseAllIO [] = throwError $ OtherError "No input files"
parseAllIO [path] = do
    content <- readFileIO path
    case parse (parseExpr <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr
parseAllIO (path : paths) = do
    content <- readFileIO path
    parsed <- parseAllIO paths
    case parse (parsePartialExpr parsed <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr

resolveAllIO :: ParsedExpr -> ConfigIO ResolvedExpr
resolveAllIO = pure . resolveNames

inferTypeIO :: ResolvedExpr -> IdentEnvIO TypedExpr
inferTypeIO expr = do
    case result of
        Left err -> lift $ throwError err
        Right typedExpr -> do
            loadTypeEnv env
            return typedExpr
  where
    (result, env) = inferType expr

kNormalizeIO :: TypedExpr -> IdentEnvIO KExpr
kNormalizeIO = kNormalize

flattenExprIO :: KExpr -> IdentEnvIO KExpr
flattenExprIO expr = expandArrayCreate (flattenExpr expr)

optimIO :: FilePath -> KExpr -> IdentEnvIO KExpr
optimIO path expr = do
    inliningSize <- asks cInliningSize
    recInliningSize <- asks cRecInliningSize
    maxInlining <- asks cMaxInlining
    evalStateT (optimIOLoop 1 expr) (OptimContext inliningSize maxInlining recInliningSize)
  where
    optimIOLoop :: Int -> KExpr -> OptimStateT ConfigIO KExpr
    optimIOLoop roundCount beforeExpr = do
        lift $ lift $ printTextLog Debug $ "Optimization round " <> pack (show roundCount) <> " started"

        -- Perform validation.
        validationResult <- lift $ validate beforeExpr
        case validationResult of
            Left err -> throwError err
            Right _ -> pure ()

        -- Register constants.
        lift $ registerConstants beforeExpr

        -- Get activated optimizations.
        optimizations <- asks cActivatedOptim

        optimExpr <-
            foldM
                ( \e kind -> do
                    optimExpr <- runOptim kind e
                    lift $ lift $ printTextLog Info $ display kind <> " performed"
                    when (optimExpr == e) $ do
                        lift $ lift $ printLog Info "Make no change"
                    pure optimExpr
                )
                beforeExpr
                optimizations
        if beforeExpr == optimExpr
            then do
                _ <- lift $ lift $ printLog Info "No space for optimization"
                pure optimExpr
            else do
                _ <- lift $ lift $ printLog Info "Perform more optimization"

                emitOptim <- lift $ asks cEmitOptim
                when emitOptim $ do
                    lift $ liftIO $ TIO.writeFile (changeExt (show roundCount ++ ".optim.ml") path) $ display optimExpr
                    lift $ lift $ printLog Debug "Optimized expressions are saved"

                optimIOLoop (roundCount + 1) optimExpr

extractGlobalsIO :: KExpr -> IdentEnvIO (KExpr, GlobalTable)
extractGlobalsIO expr =
    runStateT
        ( do
            expr' <- extractGlobals expr
            globalInfo <- reportGlobals
            mapM_ (lift . lift . printTextLog Debug) globalInfo
            pure expr'
        )
        defaultGlobalTable

getFunctionsIO :: KExpr -> IdentEnvIO [Function]
getFunctionsIO = getFunctions

toInstructionsIO :: [Function] -> BackendIdentStateIO [IntermediateCodeBlock Loc Int]
toInstructionsIO = mapM toInstructions

optimInstIO :: [IntermediateCodeBlock Loc RegID] -> BackendIdentStateIO [IntermediateCodeBlock Loc RegID]
optimInstIO inst = do
    evalStateT (mapM (optimInstIOLoop 1) inst) BackEndOptimContext
  where
    optimInstIOLoop :: Int -> IntermediateCodeBlock Loc RegID -> BackEndOptimStateT (IdentEnvT ConfigIO) (IntermediateCodeBlock Loc RegID)
    optimInstIOLoop roundCount beforeInst = do
        lift $ liftB $ lift $ printTextLog Debug $ "Optimization round " <> pack (show roundCount) <> " started"

        -- Get activated optimizations.
        optimizations <- lift $ liftB $ asks cActivatedBackEndOptim

        optimInst <-
            foldM
                ( \e kind -> do
                    optimInst <- runBackEndOptim kind e
                    lift $ liftB $ lift $ printTextLog Info $ display kind <> " performed"
                    when (optimInst == e) $ do
                        lift $ liftB $ lift $ printLog Info "Make no change"
                    pure optimInst
                )
                beforeInst
                optimizations
        if beforeInst == optimInst
            then do
                _ <- lift $ liftB $ lift $ printLog Info "No space for optimization"
                pure optimInst
            else do
                _ <- lift $ liftB $ lift $ printLog Info "Perform more optimization"
                optimInstIOLoop (roundCount + 1) optimInst

transformCodeBlockIO :: [IntermediateCodeBlock Loc Int] -> BackendIdentStateIO [CodeBlock Loc Int]
transformCodeBlockIO blocks =
    (\blocks' -> blocks' ++ [exitBlock]) . concat
        <$> mapM
            ( \block -> do
                blocks' <- transformCodeBlock block
                liftB $ lift $ printTextLog Debug $ "Generated " <> getICBLabel block
                pure blocks'
            )
            blocks

livenessIO :: [IntermediateCodeBlock Loc Int] -> BackendIdentStateIO [IntermediateCodeBlock LivenessLoc Int]
livenessIO = mapM livenessIO'
  where
    livenessIO' :: IntermediateCodeBlock Loc Int -> BackendIdentStateIO (IntermediateCodeBlock LivenessLoc Int)
    livenessIO' block =
        pure $
            block{getICBInst = liveness $ getICBInst block}

assignRegisterIO :: [IntermediateCodeBlock LivenessLoc RegID] -> BackendIdentStateIO [IntermediateCodeBlock Loc Int]
assignRegisterIO blocks = do
    -- Report used registers.
    usedIRegLen' <- gets (generatedReg . (#!! RInt) . regContext)
    usedFRegLen' <- gets (generatedReg . (#!! RFloat) . regContext)
    liftB $ lift $ printLog Debug $ "Before: int: " <> show usedIRegLen' <> ", float: " <> show usedFRegLen'

    blocks' <-
        mapM
            ( \block -> do
                liftB $ lift $ printTextLog Debug $ "Allocating registers for " <> getICBLabel block

                block' <- assignRegisterLoop block

                liftB $ lift $ printTextLog Debug $ "Allocated registers for " <> getICBLabel block
                pure block'
            )
            blocks

    -- Report used registers.
    usedRegLen' <- gets ((VariantItem . generatedReg #$) . regContext)
    liftB $ lift $ printLog Debug $ "After: int: " <> show (usedRegLen' #!! RInt) <> ", float: " <> show (usedRegLen' #!! RFloat)

    -- Perform register saving.
    let blocks'' = map saveRegisters blocks'
    liftB $ lift $ printLog Debug "Refuge registers beyond function calls"

    pure blocks''
  where
    assignRegisterLoop :: IntermediateCodeBlock LivenessLoc RegID -> BackendIdentStateIO (IntermediateCodeBlock Loc RegID)
    assignRegisterLoop block = do
        let ~(used, spillTarget, block') = assignRegister block
        let usedI = unwrap $ used #!! RInt
        let usedF = unwrap $ used #!! RFloat
        let iSpillTarget = unwrap $ spillTarget #!! RInt
        let fSpillTarget = unwrap $ spillTarget #!! RFloat

        liftB $
            lift $
                printTextLog Debug $
                    "Required registers: "
                        <> pack (show (used #!! RInt))
                        <> ", "
                        <> pack (show (used #!! RFloat))

        iLimit <- asks (regLimit . (#!! RInt) . regConfig)
        if iLimit < usedI
            then do
                case iSpillTarget of
                    Just target -> do
                        let livenessRemoved = map (substIState livenessLoc) $ getICBInst block
                        spilt <- spillI target block{getICBInst = livenessRemoved}

                        liftB $ lift $ printTextLog Debug $ "Register spilt: " <> display (savedReg RInt target)

                        assignRegisterLoop spilt{getICBInst = liveness $ getICBInst spilt}
                    Nothing -> do
                        throwError $ OtherError "Failed to find a spill target for int registers."
            else do
                fLimit <- asks (regLimit . (#!! RFloat) . regConfig)
                if fLimit < usedF
                    then do
                        case fSpillTarget of
                            Just target -> do
                                let livenessRemoved = map (substIState livenessLoc) $ getICBInst block
                                spilt <- spillF target block{getICBInst = livenessRemoved}

                                liftB $ lift $ printTextLog Debug $ "Register spilt: " <> display (savedReg RFloat target)

                                assignRegisterLoop spilt{getICBInst = liveness $ getICBInst spilt}
                            Nothing -> do
                                throwError $ OtherError "Failed to find a spill target for float registers."
                    else do
                        modify $ \env ->
                            env
                                { regContext = updateVariant RInt (\ctx -> ctx{usedRegLen = max usedI (usedRegLen ctx)}) $ regContext env
                                }
                        modify $ \env ->
                            env
                                { regContext = updateVariant RFloat (\ctx -> ctx{usedRegLen = max usedF (usedRegLen ctx)}) $ regContext env
                                }
                        pure block'

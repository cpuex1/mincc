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

import ArrayCreate (expandArrayCreate)
import Backend.Asm (
    CodeBlock,
    IntermediateCodeBlock (getICBInst, getICBLabel),
    RegID,
    Register (SavedReg),
    exitBlock,
    substIState,
 )
import Backend.BackendEnv (
    BackendConfig (fRegLimit, iRegLimit),
    BackendEnv (generatedFReg, generatedIReg, usedFRegLen, usedIRegLen),
    liftB,
 )
import Backend.FunctionCall (saveRegisters)
import Backend.Liveness (LivenessLoc (livenessLoc), liveness)
import Backend.Lowering (toInstructions)
import Backend.Optim.MulElim (elimMul)
import Backend.RegisterAlloc (assignRegister)
import Backend.Spill (spillF, spillI)
import Backend.Transform (transformCodeBlock)
import Closure (getFunctions)
import CommandLine (BackendIdentStateIO, ConfigIO, IdentEnvIO)
import Control.Exception (IOException, try)
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (StateT (runStateT), evalStateT, gets, modify)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Display (display)
import Error (CompilerError (OtherError, ParserError))
import Flatten (flattenExpr)
import Globals (GlobalTable, defaultGlobalTable, extractGlobals, reportGlobals)
import IdentAnalysis (loadTypeEnv)
import KNorm (kNormalize)
import Log (LogLevel (Debug, Info), printLog, printTextLog)
import NameRes (resolveNames)
import Optim.Base (OptimContext (OptimContext), OptimStateT)
import Optim.CompMerging (runMergeComp)
import Optim.ConstFold (constFold)
import Optim.Inlining (runInlining)
import Optim.UnitElim (elimUnitArgs)
import Optim.UnusedElim (unusedElim)
import Parser (parseExpr, parsePartialExpr)
import Syntax (
    Function,
    KExpr,
    Loc,
    ParsedExpr,
    ResolvedExpr,
    TypedExpr,
 )
import Text.Megaparsec (MonadParsec (eof), parse)
import TypeInferrer (inferType)

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

optimChain :: (Monad m) => [(Text, KExpr -> OptimStateT m KExpr)]
optimChain =
    [ ("Function inlining", runInlining)
    , ("Constant folding", constFold)
    , ("Comparison merging", runMergeComp)
    , ("Unused variables elimination", unusedElim)
    , ("Unit args elimination", elimUnitArgs)
    ]

optimIO :: KExpr -> IdentEnvIO KExpr
optimIO expr = evalStateT (optimIOLoop expr) (OptimContext 470 0 0)
  where
    optimIOLoop :: KExpr -> OptimStateT ConfigIO KExpr
    optimIOLoop beforeExpr = do
        optimExpr <-
            foldM
                ( \e (name, optim) -> do
                    optimExpr <- optim e
                    lift $ lift $ printTextLog Info $ name <> " performed"
                    when (optimExpr == e) $ do
                        lift $ lift $ printLog Info "Make no change"
                    pure optimExpr
                )
                beforeExpr
                optimChain
        if beforeExpr == optimExpr
            then do
                _ <- lift $ lift $ printLog Info "No space for optimization"
                pure optimExpr
            else do
                _ <- lift $ lift $ printLog Info "Perform more optimization"
                optimIOLoop optimExpr

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
optimInstIO inst = pure $ map elimMul inst

transformCodeBlockIO :: [IntermediateCodeBlock Loc Int] -> BackendIdentStateIO [CodeBlock Loc Int]
transformCodeBlockIO blocks = (\blocks' -> blocks' ++ [exitBlock]) . concat <$> mapM transformCodeBlock blocks

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
    usedIRegLen' <- gets generatedIReg
    usedFRegLen' <- gets generatedFReg
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
    usedIRegLen'' <- gets usedIRegLen
    usedFRegLen'' <- gets usedFRegLen
    liftB $ lift $ printLog Debug $ "After: int: " <> show usedIRegLen'' <> ", float: " <> show usedFRegLen''

    -- Perform register saving.
    let blocks'' = map saveRegisters blocks'
    liftB $ lift $ printLog Debug "Refuge registers beyond function calls"

    pure blocks''
  where
    assignRegisterLoop :: IntermediateCodeBlock LivenessLoc RegID -> BackendIdentStateIO (IntermediateCodeBlock Loc RegID)
    assignRegisterLoop block = do
        (usedI, usedF, iSpillTarget, fSpillTarget, block') <- assignRegister block

        liftB $
            lift $
                printTextLog Debug $
                    "Required registers: "
                        <> pack (show usedI)
                        <> ", "
                        <> pack (show usedF)

        iLimit <- asks iRegLimit
        if iLimit < usedI
            then do
                case iSpillTarget of
                    Just target -> do
                        let livenessRemoved = map (substIState livenessLoc) $ getICBInst block
                        spilt <- spillI target block{getICBInst = livenessRemoved}

                        liftB $ lift $ printTextLog Debug $ "Register spilt: " <> display (SavedReg target :: Register RegID Int)

                        assignRegisterLoop spilt{getICBInst = liveness $ getICBInst spilt}
                    Nothing -> do
                        throwError $ OtherError "Failed to find a spill target for int registers."
            else do
                fLimit <- asks fRegLimit
                if fLimit < usedF
                    then do
                        case fSpillTarget of
                            Just target -> do
                                let livenessRemoved = map (substIState livenessLoc) $ getICBInst block
                                spilt <- spillF target block{getICBInst = livenessRemoved}

                                liftB $ lift $ printTextLog Debug $ "Register spilt: " <> display (SavedReg target :: Register RegID Float)

                                assignRegisterLoop spilt{getICBInst = liveness $ getICBInst spilt}
                            Nothing -> do
                                throwError $ OtherError "Failed to find a spill target for float registers."
                    else do
                        modify $ \env ->
                            env
                                { usedIRegLen = max usedI (usedIRegLen env)
                                , usedFRegLen = max usedF (usedFRegLen env)
                                }
                        pure block'

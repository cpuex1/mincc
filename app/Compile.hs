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
import Backend.Asm
import Backend.BackendEnv (
    BackendConfig (fRegLimit, iRegLimit),
    BackendEnv (generatedFReg, generatedIReg, usedFRegLen, usedIRegLen),
    liftB,
 )
import Backend.FunctionCall (saveRegisters)
import Backend.Liveness (LivenessLoc (livenessLoc), liveness)
import Backend.Lowering
import Backend.Optim.MulElim (elimMul)
import Backend.RegisterAlloc (assignRegister)
import Backend.Spill (spillF, spillI)
import Backend.Transform (transformCodeBlock)
import Closure (getFunctions)
import CommandLine
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (StateT (runStateT), gets, modify)
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Display (display)
import Error
import Flatten (flattenExpr)
import Globals (GlobalTable, defaultGlobalTable, extractGlobals, reportGlobals)
import IdentAnalysis (IdentEnvT, loadTypeEnv)
import KNorm
import Log
import NameRes (resolveNames)
import Optim.CompMerging (runMergeComp)
import Optim.ConstFold (constFold)
import Parser
import Syntax
import Text.Megaparsec
import TypeInferrer (inferType)

parseAllIO :: [FilePath] -> ConfigIO ParsedExpr
parseAllIO [] = throwError $ OtherError "No input files"
parseAllIO [path] = do
    content <- liftIO $ B.readFile path
    case parse (parseExpr <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr
parseAllIO (path : paths) = do
    content <- liftIO $ B.readFile path
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

optimChain :: (Monad m) => [(Text, KExpr -> IdentEnvT m KExpr)]
optimChain =
    [ ("Constant folding", constFold)
    , ("Comparison merging", runMergeComp)
    ]

optimIO :: KExpr -> Int -> IdentEnvIO KExpr
optimIO expr limit = do
    if limit == 0
        then
            pure expr
        else do
            expr' <-
                foldM
                    ( \e (name, optim) -> do
                        expr' <- optim e
                        _ <- lift $ printTextLog Info $ name <> " performed"
                        when (expr' == e) $ do
                            lift $ printLog Info "Make no change"
                        pure expr'
                    )
                    expr
                    optimChain
            if expr == expr'
                then do
                    _ <- lift $ printLog Info "No space for optimization"
                    pure expr'
                else do
                    _ <- lift $ printLog Info "Perform more optimization"
                    optimIO expr' (limit - 1)

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

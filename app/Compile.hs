{-# LANGUAGE OverloadedStrings #-}

module Compile (
    parseAllIO,
    resolveAllIO,
    inferTypeIO,
    kNormalizeIO,
    flattenExprIO,
    getFunctionsIO,
    toInstructionsIO,
    transformCodeBlockIO,
    livenessIO,
    assignRegisterIO,
) where

import Backend.Asm
import Backend.BackendEnv (BackendEnv (generatedFReg, generatedIReg, usedFRegLen, usedIRegLen), liftB)
import Backend.Liveness (LivenessLoc, liveness)
import Backend.Lowering
import Backend.RegisterAlloc (assignRegister)
import Backend.Transform (transformCodeBlock)
import Closure (getFunctions)
import CommandLine
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Lazy (gets)
import Control.Monad.Trans.Class
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Error
import Flatten (flattenExpr)
import IdentAnalysis (loadTypeEnv)
import KNorm
import Log
import NameRes (resolveNames)
import Parser
import Syntax
import Text.Megaparsec
import TypeInferrer (inferType)

parseIO :: FilePath -> ConfigIO ParsedExpr
parseIO path = do
    content <- liftIO $ B.readFile path
    case parse (parseExpr <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr

parseAllIO :: [FilePath] -> ConfigIO [ParsedExpr]
parseAllIO = mapM parseIO

resolveAllIO :: [ParsedExpr] -> ConfigIO [ResolvedExpr]
resolveAllIO exprs = pure $ map resolveNames exprs

inferTypeIO :: [ResolvedExpr] -> IdentEnvIO [TypedExpr]
inferTypeIO [] = pure []
inferTypeIO (expr : rest) = do
    case result of
        Left err -> lift $ throwError err
        Right typedExpr -> do
            rest' <- inferTypeIO rest
            loadTypeEnv env
            return (typedExpr : rest')
  where
    (result, env) = inferType expr

kNormalizeIO :: [TypedExpr] -> IdentEnvIO [KExpr]
kNormalizeIO = mapM kNormalize

flattenExprIO :: [KExpr] -> IdentEnvIO [KExpr]
flattenExprIO exprs = pure $ map flattenExpr exprs

getFunctionsIO :: [KExpr] -> IdentEnvIO [Function]
getFunctionsIO [] = pure []
getFunctionsIO (expr : exprs) = do
    func <- getFunctions expr
    functions <- getFunctionsIO exprs
    pure (func ++ functions)

toInstructionsIO :: [Function] -> BackendIdentStateIO [IntermediateCodeBlock Loc Int]
toInstructionsIO = mapM toInstructions

transformCodeBlockIO :: [IntermediateCodeBlock Loc Int] -> BackendIdentStateIO [CodeBlock Loc Int]
transformCodeBlockIO = pure . (\blocks -> blocks ++ [exitBlock]) . concatMap transformCodeBlock

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

                block' <- assignRegister block

                liftB $ lift $ printTextLog Debug $ "Allocated registers for " <> getICBLabel block
                pure block'
            )
            blocks

    -- Report used registers.
    usedIRegLen'' <- gets usedIRegLen
    usedFRegLen'' <- gets usedFRegLen
    liftB $ lift $ printLog Debug $ "After: int: " <> show usedIRegLen'' <> ", float: " <> show usedFRegLen''
    pure blocks'

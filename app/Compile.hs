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
import Backend.Liveness (LivenessLoc (LivenessLoc), LivenessState (LivenessState), liveness)
import Backend.Lowering
import Backend.RegisterAlloc (assignRegister)
import Backend.Transform (transformCodeBlock)
import Closure (getFunctions)
import CommandLine
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (evalStateT)
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
    -- Since the parser uses Identity monad and StateT monad, we should unwrap them.
    case runIdentity $ evalStateT (runParserT (parseExpr <* eof) path $ decodeUtf8 content) 0 of
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
    livenessIO' (IntermediateCodeBlock label' prologue inst epilogue) =
        pure $
            IntermediateCodeBlock
                label'
                prologue'
                (liveness inst)
                epilogue'
      where
        prologue' = map (substIState toLivenessLoc) prologue
        epilogue' = map (substIState toLivenessLoc) epilogue

    toLivenessLoc :: Loc -> LivenessLoc
    toLivenessLoc loc = LivenessLoc loc (LivenessState [] [])

assignRegisterIO :: [IntermediateCodeBlock LivenessLoc RegID] -> BackendIdentStateIO [IntermediateCodeBlock Loc Int]
assignRegisterIO = mapM assignRegister

module Compile (
    parseAllIO,
    resolveAllIO,
    inferTypeIO,
    kNormalizeIO,
    flattenExprIO,
    getFunctionsIO,
    loadFunctionsIO,
) where

import Asm
import Backend
import Closure (getFunctions)
import CommandLine
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (runState)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (asks)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Text.IO as TIO
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
    content <- lift $ lift $ TIO.readFile path
    case parse (parseExpr <* eof) path content of
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

kNormalizeIO :: [TypedExpr] -> IdentEnvIO [(KExpr, OptimEnv)]
kNormalizeIO exprs = pure $ map (\expr -> runState (kNormalize expr) defaultOptimEnv) exprs

flattenExprIO :: [(KExpr, OptimEnv)] -> IdentEnvIO [(KExpr, OptimEnv)]
flattenExprIO exprs = pure $ map (first flattenExpr) exprs

getFunctionsIO :: [KExpr] -> IdentEnvIO [Function]
getFunctionsIO exprs = pure $ concatMap getFunctions exprs

loadFunctionsIO :: [Function] -> IdentEnvIO [CodeBlock Loc Int]
loadFunctionsIO functions = do
    argsLimit' <- lift $ asks cArgsLimit
    case loadFunctions (BackendConfig argsLimit') functions of
        Left err -> lift $ throwError err
        Right codeBlocks -> pure codeBlocks

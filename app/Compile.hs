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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (asks)
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

loadFunctionsIO :: [Function] -> IdentEnvIO [CodeBlock Loc Int]
loadFunctionsIO functions = do
    argsLimit' <- lift $ asks cArgsLimit
    functions' <- loadFunctions (BackendConfig argsLimit') functions
    case functions' of
        Left err -> lift $ throwError err
        Right codeBlocks -> pure codeBlocks

module Compile (
    parseAllIO,
    resolveAllIO,
    inferTypeIO,
    kNormalizeIO,
    flattenExprIO,
    getFunctionsIO
) where

import CommandLine
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (runState)
import Control.Monad.Trans.Class
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Text.IO as TIO
import Error
import Flatten (flattenExpr)
import KNorm
import Log
import NameRes (resolveNames)
import Parser
import Syntax
import Text.Megaparsec
import TypeInferrer (inferType)
import Closure (getFunctions)

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

inferTypeIO :: [ResolvedExpr] -> ConfigIO [TypedExpr]
inferTypeIO [] = pure []
inferTypeIO (expr : rest) = do
    case inferType expr of
        Left err -> throwError err
        Right typedExpr -> do
            rest' <- inferTypeIO rest
            return (typedExpr : rest')

kNormalizeIO :: [TypedExpr] -> ConfigIO [(KExpr, OptimEnv)]
kNormalizeIO exprs = pure $ map (\expr -> runState (kNormalize expr) defaultOptimEnv) exprs

flattenExprIO :: [(KExpr, OptimEnv)] -> ConfigIO [(KExpr, OptimEnv)]
flattenExprIO exprs = pure $ map (first flattenExpr) exprs

getFunctionsIO :: [KExpr] -> ConfigIO [Function]
getFunctionsIO exprs = pure $ concatMap getFunctions exprs

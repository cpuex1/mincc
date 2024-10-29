module Compile (parseAllIO, resolveAllIO, inferTypeIO, kNormalizeIO) where

import CommandLine
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Trans.Class
import qualified Data.Text.IO as TIO
import Error
import Log
import NameRes (resolveNames)
import Parser
import Syntax
import Text.Megaparsec
import TypeInferrer (inferType)
import KNorm
import Control.Monad.State (runState)

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

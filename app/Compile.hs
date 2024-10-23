module Compile (compileAll) where

import CommandLine
import Control.Monad.Trans.Class
import qualified Data.Text.IO as TIO
import Error
import Log
import Parser
import Syntax
import Text.Megaparsec

compile :: FilePath -> ArgIO (Maybe Expr)
compile path = do
    content <- lift $ TIO.readFile path
    case parse (parseExpr <* eof) path content of
        Left err -> do
            mapM_ (printTextLog Error) $ displayError (ParserError err)
            return Nothing
        Right expr ->
            return (Just expr)

compileAll :: [FilePath] -> ArgIO (Maybe [Expr])
compileAll paths = do
    exprs <- mapM compile paths
    return $ allValid exprs
  where
    allValid :: [Maybe Expr] -> Maybe [Expr]
    allValid [] = Just []
    allValid (Just x : xs) = (x :) <$> allValid xs
    allValid _ = Nothing

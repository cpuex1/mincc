{-# LANGUAGE OverloadedStrings #-}

module Compile (compile) where

import qualified Data.Text.IO as TIO
import Error
import Parser
import Syntax
import Text.Megaparsec

compile :: FilePath -> IO (Maybe Expr)
compile path = do
    content <- TIO.readFile path
    case parse parseExpr path content of
        Left err -> do
            mapM_ TIO.putStrLn $ displayError (ParserError err)
            return Nothing
        Right expr ->
            return (Just expr)

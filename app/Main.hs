{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CommandLine
import Compile
import Control.Monad.Trans.Reader
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import Display (displayExpr)
import Options.Applicative

import Control.Monad.Trans.Class
import Log

main :: IO ()
main = execParser opts >>= \arg -> runReaderT execArgs arg
  where
    opts =
        info
            (parseArg <**> helper)
            ( fullDesc
                <> progDesc "MinCaml ---[Mincc]---> RINANA"
                <> header "mincc (c) 2024 Akihisa Yagi\n\n"
            )

execArgs :: ArgIO ()
execArgs = do
    inputFiles <- asks input
    outputFile <- asks output
    parsedExprs <- compileAll inputFiles
    case parsedExprs of
        Just exprs -> do
            printLog Info "Parsing succeeded"
            lift $ TIO.writeFile outputFile $ intercalate "\n" $ map displayExpr exprs
            printLog Info "Compilation succeeded"
        Nothing ->
            printLog Info "Compilation failed"

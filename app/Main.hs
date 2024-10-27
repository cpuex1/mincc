{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CommandLine
import Compile
import Control.Monad.Trans.Reader
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import Display
import Options.Applicative

import Control.Monad.Trans.Class
import Log
import NameRes (resolveNames)
import Path

main :: IO ()
main = do
    args <- execParser opts
    config <- toCompilerConfig args
    runReaderT execArgs config
  where
    opts =
        info
            (parseArg <**> helper)
            ( fullDesc
                <> progDesc "MinCaml ---[Mincc]---> RINANA"
                <> header "mincc (c) 2024 Akihisa Yagi\n\n"
            )

execArgs :: ConfigIO ()
execArgs = do
    ansi <- asks cANSI
    if ansi
        then
            printLog Debug "ANSI enabled"
        else
            printLog Debug "ANSI disabled"
    inputFiles <- asks cInput
    if any ((\ext -> ext /= Just "ml") . getExt) inputFiles
        then
            printLog Error "An input file must have a .ml extension"
        else do
            outputFile <- asks cOutput
            if getExt outputFile /= Just "s"
                then
                    printLog Error "An output file must have a .s extension"
                else do
                    parsedExprs <- compileAll inputFiles
                    case parsedExprs of
                        Just exprs -> do
                            printLog Done "Parsing succeeded"
                            lift $ TIO.writeFile (changeExt "parsed.ml" outputFile) $ intercalate "\n" $ map display exprs
                            lift $ TIO.writeFile (changeExt "resolved.ml" outputFile) $ intercalate "\n" $ map (display . resolveNames) exprs
                            printLog Done "Name resolution succeeded"
                            printLog Done "Compilation succeeded"
                        Nothing ->
                            printLog Error "Compilation failed"

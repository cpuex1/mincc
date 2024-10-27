{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CommandLine
import Compile
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import Display
import Options.Applicative

import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.Trans.Class
import Error
import Log
import Path

main :: IO ()
main = do
    args <- execParser opts
    config <- toCompilerConfig args
    _ <- runExceptT $ runReaderT execArgs config
    pure ()
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
    catchError
        ( do
            ansi <- asks cANSI
            printLog Debug (if ansi then "ANSI enabled" else "ANSI disabled")

            inputFiles <- asks cInput
            when (any ((\ext -> ext /= Just "ml") . getExt) inputFiles) $
                throwError $
                    OtherError "An input file must have a .ml extension"

            outputFile <- asks cOutput
            when (getExt outputFile /= Just "s") $
                throwError $
                    OtherError "An output file must have a .s extension"

            parsedExprs <- parseAllIO inputFiles
            printLog Done "Parsing succeeded"
            lift $ lift $ TIO.writeFile (changeExt "parsed.ml" outputFile) $ intercalate "\n" $ map display parsedExprs
            printLog Debug "Parsed expressions are saved"

            resolvedExprs <- resolveAllIO parsedExprs
            printLog Done "Name resolution succeeded"
            lift $ lift $ TIO.writeFile (changeExt "resolved.ml" outputFile) $ intercalate "\n" $ map display resolvedExprs
            printLog Debug "Resolved expressions are saved"

            typedExprs <- inferTypeIO resolvedExprs
            printLog Done "Type inference succeeded"
            lift $ lift $ TIO.writeFile (changeExt "typed.ml" outputFile) $ intercalate "\n" $ map display typedExprs
            printLog Debug "Typed expressions are saved"

            printLog Done "Compilation succeeded"
        )
        $ \err -> do
            mapM_ (printTextLog Error) $ displayError err
            printLog Error "Compilation failed"

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
            emitParsed <- asks cEmitParsed
            when emitParsed $ do
                lift $ lift $ TIO.writeFile (changeExt "parsed.ml" outputFile) $ intercalate "\n" $ map display parsedExprs
                printLog Debug "Parsed expressions are saved"

            resolvedExprs <- resolveAllIO parsedExprs
            printLog Done "Name resolution succeeded"
            emitResolved <- asks cEmitResolved
            when emitResolved $ do
                lift $ lift $ TIO.writeFile (changeExt "resolved.ml" outputFile) $ intercalate "\n" $ map display resolvedExprs
                printLog Debug "Resolved expressions are saved"

            typedExprs <- inferTypeIO resolvedExprs
            printLog Done "Type inference succeeded"
            emitTyped <- asks cEmitTyped
            when emitTyped $ do
                lift $ lift $ TIO.writeFile (changeExt "typed.ml" outputFile) $ intercalate "\n" $ map display typedExprs
                printLog Debug "Typed expressions are saved"

            kExprs <- kNormalizeIO typedExprs
            printLog Done "K-normalization succeeded"
            emitKNorm <- asks cEmitKNorm
            when emitKNorm $ do
                lift $ lift $ TIO.writeFile (changeExt "norm.ml" outputFile) $ intercalate "\n" $ map (display . fst) kExprs
                printLog Debug "K-normalized expressions are saved"

            flattenExprs <- flattenExprIO kExprs
            printLog Done "Flatten succeeded"
            emitFlatten <- asks cEmitFlatten
            when emitFlatten $ do
                lift $ lift $ TIO.writeFile (changeExt "flatten.ml" outputFile) $ intercalate "\n" $ map (display . fst) flattenExprs
                printLog Debug "Flatten expressions are saved"

            functions <- getFunctionsIO (map fst flattenExprs)
            printLog Done "Closure conversion succeeded"
            emitClosure <- asks cEmitClosure
            when emitClosure $ do
                lift $ lift $ TIO.writeFile (changeExt "closure.ml" outputFile) $ intercalate "\n" $ map display functions
                printLog Debug "Closure expressions are saved"

            blocks <- loadFunctionsIO functions
            printLog Done "Code generation succeeded"
            lift $ lift $ TIO.writeFile (changeExt "code.s" outputFile) $ intercalate "\n" $ map display blocks
            printLog Debug "Generated code was saved"

            printLog Done "Compilation succeeded"
        )
        $ \err -> do
            mapM_ (printTextLog Error) $ displayError err
            printLog Error "Compilation failed"

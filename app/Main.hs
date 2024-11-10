{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CommandLine
import Compile
import Control.Monad (void, when)
import Control.Monad.Trans.Reader
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import Display
import Options.Applicative

import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Error
import IdentAnalysis (IdentEnvT (runIdentEnvT), defaultIdentE, reportEnv)
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

            void $
                runIdentEnvT
                    ( do
                        typedExprs <- inferTypeIO resolvedExprs
                        lift $ printLog Done "Type inference succeeded"
                        emitTyped <- lift $ asks cEmitTyped
                        when emitTyped $ do
                            liftIO $ TIO.writeFile (changeExt "typed.ml" outputFile) $ intercalate "\n" $ map display typedExprs
                            lift $ printLog Debug "Typed expressions are saved"

                        reported <- reportEnv
                        lift $ mapM_ (printTextLog Debug) reported

                        kExprs <- kNormalizeIO typedExprs
                        lift $ printLog Done "K-normalization succeeded"
                        emitKNorm <- lift $ asks cEmitKNorm
                        when emitKNorm $ do
                            liftIO $ TIO.writeFile (changeExt "norm.ml" outputFile) $ intercalate "\n" $ map display kExprs
                            lift $ printLog Debug "K-normalized expressions are saved"

                        flattenExprs <- flattenExprIO kExprs
                        lift $ printLog Done "Flatten succeeded"
                        emitFlatten <- lift $ asks cEmitFlatten
                        when emitFlatten $ do
                            liftIO $ TIO.writeFile (changeExt "flatten.ml" outputFile) $ intercalate "\n" $ map display flattenExprs
                            lift $ printLog Debug "Flatten expressions are saved"

                        functions <- getFunctionsIO flattenExprs
                        lift $ printLog Done "Closure conversion succeeded"
                        emitClosure <- lift $ asks cEmitClosure
                        when emitClosure $ do
                            liftIO $ TIO.writeFile (changeExt "closure.ml" outputFile) $ intercalate "\n" $ map display functions
                            lift $ printLog Debug "Closure expressions are saved"

                        blocks <- loadFunctionsIO functions
                        lift $ printLog Done "Code generation succeeded"
                        liftIO $ TIO.writeFile (changeExt "code.s" outputFile) $ intercalate "\n" $ map display blocks
                        lift $ printLog Debug "Generated code was saved"

                        lift $ printLog Done "Compilation succeeded"
                    )
                    defaultIdentE
        )
        $ \err -> do
            mapM_ (printTextLog Error) $ displayError err
            printLog Error "Compilation failed"

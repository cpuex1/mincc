{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CommandLine
import Compile
import Control.Monad (when)
import Control.Monad.Trans.Reader
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import Display
import Options.Applicative (
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
 )

import Backend.BackendEnv (BackendConfig (BackendConfig), liftB, runBackendStateT)
import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Error
import IdentAnalysis (IdentEnvT (runIdentEnvT), defaultIdentE, reportEnv)
import Log
import Path
import Syntax (Function, ResolvedExpr)
import System.Exit (exitWith, ExitCode (ExitFailure))

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

            -- Run with a monad that holds identifier information.
            _ <- runIdentEnvT (execArgsWithIdent resolvedExprs) defaultIdentE

            printLog Done "Compilation succeeded"
        )
        $ \err -> do
            mapM_ (printTextLog Error) $ displayError err
            printLog Error "Compilation failed"
            liftIO $ exitWith $ ExitFailure 1

execArgsWithIdent :: [ResolvedExpr] -> IdentEnvIO ()
execArgsWithIdent resolvedExprs = do
    outputFile <- lift $ asks cOutput

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

    argsLimit' <- lift $ asks cArgsLimit
    blocks <- runBackendStateT (execArgsWithBackend functions) $ BackendConfig argsLimit'
    case blocks of
        Left err -> lift $ throwError err
        Right _ -> pure ()

execArgsWithBackend :: [Function] -> BackendIdentStateIO ()
execArgsWithBackend functions = do
    outputFile <- liftB' $ asks cOutput

    blocks <- toInstructionsIO functions
    liftB' $ printLog Done "Lowering succeeded"
    emitIR <- liftB' $ asks cEmitIR
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "ir.s" outputFile) $ intercalate "\n" $ map display blocks
        liftB' $ printLog Debug "Intermediate representation code was saved"

    liveness <- livenessIO blocks
    liftB' $ printLog Done "Liveness analysis succeeded"
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "live.s" outputFile) $ intercalate "\n" $ map display liveness
        liftB' $ printLog Debug "The result of liveness analysis was saved"

    assignedBlocks <- assignRegisterIO liveness
    liftB' $ printLog Done "Register allocation succeeded"
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "alloc.s" outputFile) $ intercalate "\n" $ map display assignedBlocks
        liftB' $ printLog Debug "The result of register allocation was saved"

    blocks' <- transformCodeBlockIO assignedBlocks
    liftB' $ printLog Done "Code generation succeeded"
    liftIO $ TIO.writeFile (changeExt "s" outputFile) $ intercalate "\n" $ map display blocks'
    liftB' $ printLog Debug "Generated code was saved"
  where
    liftB' a = liftB $ lift a

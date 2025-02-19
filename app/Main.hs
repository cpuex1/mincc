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

import BackEnd.Analysis.Liveness (runGraphLiveness)
import BackEnd.BackendEnv (createBackendConfig, liftB, runBackendStateT)
import BackEnd.Lowering (generateBlockGraph)
import BackEnd.Refuge (runRefugeBlock)
import BackEnd.RegisterAlloc (assignReg)
import CodeBlock (asMermaidGraph)
import Control.Monad.Except (MonadError (catchError, throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (lift)
import Error (CompilerError (OtherError), displayError)
import Log (LogLevel (Debug, Done, Error), printLog, printTextLog)
import MiddleEnd.Analysis.Constant (registerConstants)
import MiddleEnd.Analysis.Identifier (defaultIdentContext, reportEnv)
import Path (changeExt, getExt)
import Syntax (Function, ResolvedExpr)
import System.Exit (ExitCode (ExitFailure), exitWith)

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

            parsedExpr <- parseAllIO inputFiles
            printLog Done "Parsing succeeded"
            emitParsed <- asks cEmitParsed
            when emitParsed $ do
                lift $ lift $ TIO.writeFile (changeExt "parsed.ml" outputFile) $ display parsedExpr
                printLog Debug "Parsed expressions are saved"

            resolvedExpr <- resolveAllIO parsedExpr
            printLog Done "Name resolution succeeded"
            emitResolved <- asks cEmitResolved
            when emitResolved $ do
                lift $ lift $ TIO.writeFile (changeExt "resolved.ml" outputFile) $ display resolvedExpr
                printLog Debug "Resolved expressions are saved"

            -- Run with a monad that holds identifier information.
            evalStateT (execArgsWithIdent resolvedExpr) defaultIdentContext

            printLog Done "Compilation succeeded"
        )
        $ \err -> do
            mapM_ (printTextLog Error) $ displayError err
            printLog Error "Compilation failed"
            liftIO $ exitWith $ ExitFailure 1

execArgsWithIdent :: ResolvedExpr -> IdentEnvIO ()
execArgsWithIdent resolvedExpr = do
    outputFile <- lift $ asks cOutput

    typedExpr <- inferTypeIO resolvedExpr
    lift $ printLog Done "Type inference succeeded"
    emitTyped <- lift $ asks cEmitTyped
    when emitTyped $ do
        liftIO $ TIO.writeFile (changeExt "typed.ml" outputFile) $ display typedExpr
        lift $ printLog Debug "Typed expressions are saved"

    kExpr <- kNormalizeIO typedExpr
    lift $ printLog Done "K-normalization succeeded"
    emitKNorm <- lift $ asks cEmitKNorm
    when emitKNorm $ do
        liftIO $ TIO.writeFile (changeExt "norm.ml" outputFile) $ display kExpr
        lift $ printLog Debug "K-normalized expressions are saved"

    flattenExpr <- flattenExprIO kExpr
    lift $ printLog Done "Flatten succeeded"
    emitFlatten <- lift $ asks cEmitFlatten
    when emitFlatten $ do
        liftIO $ TIO.writeFile (changeExt "flatten.ml" outputFile) $ display flattenExpr
        lift $ printLog Debug "Flatten expressions are saved"

    registerConstants flattenExpr
    reported <- reportEnv
    lift $ mapM_ (printTextLog Debug) reported

    optimizedExpr <- optimIO outputFile flattenExpr
    lift $ printLog Done "Optimization succeeded"
    emitOptim <- lift $ asks cEmitOptim
    when emitOptim $ do
        liftIO $ TIO.writeFile (changeExt "optim.ml" outputFile) $ display optimizedExpr
        lift $ printLog Debug "Optimized expressions are saved"

    (extracted, globals) <- extractGlobalsIO optimizedExpr
    lift $ printLog Done "Global extraction succeeded"
    -- TODO: replace emitOptim with emitGlobals
    when emitOptim $ do
        liftIO $ TIO.writeFile (changeExt "globals.ml" outputFile) $ display extracted
        lift $ printLog Debug "Expressions with globals are saved"

    functions <- getFunctionsIO extracted
    lift $ printLog Done "Closure conversion succeeded"
    emitClosure <- lift $ asks cEmitClosure
    when emitClosure $ do
        liftIO $ TIO.writeFile (changeExt "closure.ml" outputFile) $ intercalate "\n" $ map display functions
        lift $ printLog Debug "Closure expressions are saved"

    iLimit' <- lift $ asks cILimit
    fLimit' <- lift $ asks cFLimit
    blocks <- runBackendStateT (execArgsWithBackend functions) (createBackendConfig iLimit' fLimit') globals
    case blocks of
        Left err -> lift $ throwError err
        Right _ -> pure ()

execArgsWithBackend :: [Function] -> BackendIdentStateIO ()
execArgsWithBackend functions = do
    outputFile <- liftB' $ asks cOutput

    blocks <- mapM generateBlockGraph functions
    liftB' $ printLog Done "Lowering succeeded"
    emitIR <- liftB' $ asks cEmitIR
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "ir.s" outputFile) $ intercalate "\n" $ map display blocks
        liftIO $ TIO.writeFile (changeExt "ir.md" outputFile) $ intercalate "\n" $ map asMermaidGraph blocks
        liftB' $ printLog Debug "Intermediate representation code was saved"

    optimBlocks <- optimInstIO blocks
    liftB' $ printLog Done "Backend optimization succeeded"
    emitOptim <- liftB' $ asks cEmitOptim
    when emitOptim $ do
        liftIO $ TIO.writeFile (changeExt "optim.s" outputFile) $ intercalate "\n" $ map display optimBlocks
        liftIO $ TIO.writeFile (changeExt "optim.md" outputFile) $ intercalate "\n" $ map asMermaidGraph optimBlocks
        liftB' $ printLog Debug "Optimized IR was saved"

    let livenessBlocks = map runGraphLiveness optimBlocks
    liftB' $ printLog Done "Liveness analysis succeeded"
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "live.s" outputFile) $ intercalate "\n" $ map display livenessBlocks
        liftB' $ printLog Debug "The result of liveness analysis was saved"

    let refuged = map runRefugeBlock livenessBlocks
    liftB' $ printLog Done "Register refuge succeeded"
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "refuge.s" outputFile) $ intercalate "\n" $ map display refuged
        liftB' $ printLog Debug "The result of register refuge was saved"

    let phiFreeBlocks = map (snd . assignReg . runGraphLiveness) refuged
    liftB' $ printLog Done "Register allocation succeeded"
    when emitIR $ do
        liftIO $ TIO.writeFile (changeExt "alloc.s" outputFile) $ intercalate "\n" $ map display phiFreeBlocks
        liftB' $ printLog Debug "The result of register allocation was saved"
  where
    -- assignedBlocks <- assignRegisterIO liveness
    -- liftB' $ printLog Done "Register allocation succeeded"
    -- when emitIR $ do
    --     liftIO $ TIO.writeFile (changeExt "alloc.s" outputFile) $ intercalate "\n" $ map display assignedBlocks
    --     liftB' $ printLog Debug "The result of register allocation was saved"

    -- blocks' <- transformCodeBlockIO assignedBlocks
    -- liftB' $ printLog Done "Code generation succeeded"
    -- liftIO $ TIO.writeFile (changeExt "s" outputFile) $ intercalate "\n" $ map display blocks'
    -- liftB' $ printLog Debug "Generated code was saved"

    liftB' = liftB . lift

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Compile (
    parseAllIO,
    resolveAllIO,
    inferTypeIO,
    kNormalizeIO,
    flattenExprIO,
    optimIO,
    extractGlobalsIO,
    getFunctionsIO,
    optimInstIO,
) where

import BackEnd.BackendEnv (liftB)
import BackEnd.Optim (runBackEndOptim)
import BackEnd.Optim.Common (BackEndOptimContext (BackEndOptimContext), BackEndOptimStateT)
import CodeBlock (VirtualBlockGraph)
import CommandLine (
    BackendIdentStateIO,
    CompilerConfig (cActivatedBackEndOptim, cActivatedOptim, cEmitOptim, cInliningSize, cMaxInlining, cRecInliningSize),
    ConfigIO,
    IdentEnvIO,
 )
import Control.Exception (IOException, try)
import Control.Monad (foldM, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks)
import Control.Monad.State.Lazy (StateT (runStateT), evalStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO as TIO
import Display (display)
import Error (CompilerError (OtherError, ParserError))
import FrontEnd.Flatten (flattenExpr)
import FrontEnd.KNorm (kNormalize)
import FrontEnd.NameRes (resolveNames)
import FrontEnd.Parser (parseExpr, parsePartialExpr)
import FrontEnd.TypeInferrer (inferType)
import Log (LogLevel (..), printLog, printTextLog)
import MiddleEnd.Analysis.Constant (registerConstants)
import MiddleEnd.Analysis.Identifier (IdentEnvT, loadTypeEnv)
import MiddleEnd.Closure (getFunctions)
import MiddleEnd.Desugar (expandArrayCreate)
import MiddleEnd.Globals (GlobalTable, defaultGlobalTable, extractGlobals, reportGlobals)
import MiddleEnd.Optim (runOptim)
import MiddleEnd.Optim.Common (
    OptimContext (OptimContext),
    OptimStateT,
 )
import MiddleEnd.Validator (validate)
import Path (changeExt)
import Syntax (
    Function,
    KExpr,
    ParsedExpr,
    ResolvedExpr,
    TypedExpr,
 )
import Text.Megaparsec (MonadParsec (eof), parse)

readFileIO :: FilePath -> ConfigIO ByteString
readFileIO path = do
    content <- liftIO (try (B.readFile path) :: IO (Either IOException ByteString))
    case content of
        Left _ -> throwError $ OtherError $ "Failed to read " <> pack path
        Right content' -> pure content'

parseAllIO :: [FilePath] -> ConfigIO ParsedExpr
parseAllIO [] = throwError $ OtherError "No input files"
parseAllIO [path] = do
    content <- readFileIO path
    case parse (parseExpr <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr
parseAllIO (path : paths) = do
    content <- readFileIO path
    parsed <- parseAllIO paths
    case parse (parsePartialExpr parsed <* eof) path $ decodeUtf8 content of
        Left err ->
            throwError $ ParserError err
        Right expr -> do
            printLog Info $ "Parsed " ++ path
            return expr

resolveAllIO :: ParsedExpr -> ConfigIO ResolvedExpr
resolveAllIO = pure . resolveNames

inferTypeIO :: ResolvedExpr -> IdentEnvIO TypedExpr
inferTypeIO expr = do
    case result of
        Left err -> lift $ throwError err
        Right typedExpr -> do
            loadTypeEnv env
            return typedExpr
  where
    (result, env) = inferType expr

kNormalizeIO :: TypedExpr -> IdentEnvIO KExpr
kNormalizeIO = kNormalize

flattenExprIO :: KExpr -> IdentEnvIO KExpr
flattenExprIO expr = expandArrayCreate (flattenExpr expr)

optimIO :: FilePath -> KExpr -> IdentEnvIO KExpr
optimIO path expr = do
    inliningSize <- asks cInliningSize
    recInliningSize <- asks cRecInliningSize
    maxInlining <- asks cMaxInlining
    evalStateT (optimIOLoop 1 expr) (OptimContext inliningSize maxInlining recInliningSize)
  where
    optimIOLoop :: Int -> KExpr -> OptimStateT ConfigIO KExpr
    optimIOLoop roundCount beforeExpr = do
        lift $ lift $ printTextLog Debug $ "Optimization round " <> pack (show roundCount) <> " started"

        -- Perform validation.
        validationResult <- lift $ validate beforeExpr
        case validationResult of
            Left err -> throwError err
            Right _ -> pure ()

        -- Register constants.
        lift $ registerConstants beforeExpr

        -- Get activated optimizations.
        optimizations <- asks cActivatedOptim

        optimExpr <-
            foldM
                ( \e kind -> do
                    optimExpr <- runOptim kind e
                    lift $ lift $ printTextLog Info $ display kind <> " performed"
                    when (optimExpr == e) $ do
                        lift $ lift $ printLog Info "Make no change"
                    pure optimExpr
                )
                beforeExpr
                optimizations
        if beforeExpr == optimExpr
            then do
                _ <- lift $ lift $ printLog Info "No space for optimization"
                pure optimExpr
            else do
                _ <- lift $ lift $ printLog Info "Perform more optimization"

                emitOptim <- lift $ asks cEmitOptim
                when emitOptim $ do
                    lift $ liftIO $ TIO.writeFile (changeExt (show roundCount ++ ".optim.ml") path) $ display optimExpr
                    lift $ lift $ printLog Debug "Optimized expressions are saved"

                optimIOLoop (roundCount + 1) optimExpr

extractGlobalsIO :: KExpr -> IdentEnvIO (KExpr, GlobalTable)
extractGlobalsIO expr =
    runStateT
        ( do
            expr' <- extractGlobals expr
            globalInfo <- reportGlobals
            mapM_ (lift . lift . printTextLog Debug) globalInfo
            pure expr'
        )
        defaultGlobalTable

getFunctionsIO :: KExpr -> IdentEnvIO [Function]
getFunctionsIO = getFunctions

optimInstIO :: [VirtualBlockGraph] -> BackendIdentStateIO [VirtualBlockGraph]
optimInstIO inst = do
    evalStateT (mapM (optimInstIOLoop 1) inst) BackEndOptimContext
  where
    optimInstIOLoop :: Int -> VirtualBlockGraph -> BackEndOptimStateT (IdentEnvT ConfigIO) VirtualBlockGraph
    optimInstIOLoop roundCount beforeInst = do
        lift $ liftB $ lift $ printTextLog Debug $ "Optimization round " <> pack (show roundCount) <> " started"

        -- Get activated optimizations.
        optimizations <- lift $ liftB $ asks cActivatedBackEndOptim

        optimInst <-
            foldM
                ( \e kind -> do
                    optimInst <- runBackEndOptim kind e
                    lift $ liftB $ lift $ printTextLog Info $ display kind <> " performed"
                    when (optimInst == e) $ do
                        lift $ liftB $ lift $ printLog Info "Make no change"
                    pure optimInst
                )
                beforeInst
                optimizations
        if beforeInst == optimInst
            then do
                _ <- lift $ liftB $ lift $ printLog Info "No space for optimization"
                pure optimInst
            else do
                _ <- lift $ liftB $ lift $ printLog Info "Perform more optimization"
                optimInstIOLoop (roundCount + 1) optimInst

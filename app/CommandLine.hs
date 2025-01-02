module CommandLine (
    ConfigIO,
    IdentEnvIO,
    BackendIdentStateIO,
    CompilerConfig (..),
    CommandLineArg (CommandLineArg),
    parseArg,
    toCompilerConfig,
) where

import Backend.Lowering (BackendIdentState)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Reader
import Data.Set (Set, insert)
import Error (CompilerError)
import IdentAnalysis (IdentEnvT)
import Optim.All (OptimKind (CompMerging, ConstFold, Inlining, UnusedElim))
import Optim.Base (Threshold, toThreshold)
import Options.Applicative
import System.Console.ANSI (hNowSupportsANSI)
import System.IO (stdout)

type ConfigIO = ReaderT CompilerConfig (ExceptT CompilerError IO)
type IdentEnvIO = IdentEnvT ConfigIO
type BackendIdentStateIO = BackendIdentState ConfigIO

defaultMaxInlining :: Int
defaultMaxInlining = 1

data CompilerConfig = CompilerConfig
    { cInput :: [String]
    , cOutput :: String
    , cVerbose :: Bool
    , cInliningSize :: Threshold
    , cRecInliningSize :: Threshold
    , cMaxInlining :: Int
    , cActivatedOptim :: Set OptimKind
    , cILimit :: Int
    , cFLimit :: Int
    , cANSI :: Bool
    , cEmitParsed :: Bool
    , cEmitResolved :: Bool
    , cEmitTyped :: Bool
    , cEmitKNorm :: Bool
    , cEmitFlatten :: Bool
    , cEmitOptim :: Bool
    , cEmitClosure :: Bool
    , cEmitIR :: Bool
    }

data CommandLineArg = CommandLineArg
    { input :: [String]
    , output :: String
    , verbose :: Bool
    , inliningSize :: Maybe Int
    , recInliningSize :: Maybe Int
    , maxInlining :: Int
    , optCompMerging :: Bool
    , optConstFold :: Bool
    , optInlining :: Bool
    , optUnusedElim :: Bool
    , optimize :: Bool
    , iLimit :: Int
    , fLimit :: Int
    , emitAll :: Bool
    , emitParsed :: Bool
    , emitResolved :: Bool
    , emitTyped :: Bool
    , emitKNorm :: Bool
    , emitFlatten :: Bool
    , emitOptim :: Bool
    , emitClosure :: Bool
    , emitIR :: Bool
    }

-- | Parse a command line argument
parseArg :: Parser CommandLineArg
parseArg =
    CommandLineArg
        <$> many
            ( strOption
                ( long "input"
                    <> short 'i'
                    <> help "Input files"
                    <> metavar "FILE"
                )
            )
        <*> strOption
            ( long "output"
                <> short 'o'
                <> help "An output file"
                <> metavar "OUTFILE"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "Whether to be verbose"
            )
        <*> option
            auto
            ( long "inlining-size"
                <> help "The maximum size of functions to be inlined. Infinity if not specified."
                <> value Nothing
                <> metavar "THRESHOLD"
            )
        <*> option
            auto
            ( long "rec-inlining-size"
                <> help "The maximum size of recursive functions to be inlined. Infinity if not specified."
                <> value Nothing
                <> metavar "THRESHOLD"
            )
        <*> option
            auto
            ( long "max-inlining"
                <> help "The maximum number of inlining. This option is only for recursive functions."
                <> showDefault
                <> value defaultMaxInlining
                <> metavar "ROUND"
            )
        <*> switch
            ( long "opt-comp-merging"
                <> help "Enable comparison merging optimization"
            )
        <*> switch
            ( long "opt-const-fold"
                <> help "Enable constant folding optimization"
            )
        <*> switch
            ( long "opt-inlining"
                <> help "Enable function inlining optimization"
            )
        <*> switch
            ( long "opt-unused-elim"
                <> help "Enable unused variable elimination optimization"
            )
        <*> switch
            ( long "optimize"
                <> short 'O'
                <> help "Enable all optimizations"
            )
        <*> option
            auto
            ( long "i-limit"
                <> help "The maximum number of int registers"
                <> showDefault
                <> value 17
                <> metavar "INT"
            )
        <*> option
            auto
            ( long "f-limit"
                <> help "The maximum number of float registers"
                <> showDefault
                <> value 20
                <> metavar "INT"
            )
        <*> switch
            ( long "emit-all"
                <> short 'e'
                <> help "Emit all intermediate files"
            )
        <*> switch
            ( long "emit-parsed"
                <> help "Emit parsed expressions"
            )
        <*> switch
            ( long "emit-resolved"
                <> help "Emit expressions after name resolution"
            )
        <*> switch
            ( long "emit-typed"
                <> help "Emit expressions after type inference"
            )
        <*> switch
            ( long "emit-knorm"
                <> help "Emit expressions after K-normalization"
            )
        <*> switch
            ( long "emit-flatten"
                <> help "Emit expressions after let-flattening"
            )
        <*> switch
            ( long "emit-optimized"
                <> help "Emit optimized expressions"
            )
        <*> switch
            ( long "emit-closure"
                <> help "Emit expressions after introducing closures"
            )
        <*> switch
            ( long "emit-ir"
                <> help "Emit intermediate representation"
            )

toCompilerConfig :: CommandLineArg -> IO CompilerConfig
toCompilerConfig arg = do
    ansiSupported <- hNowSupportsANSI stdout
    return $
        CompilerConfig
            { cInput = input arg
            , cOutput = output arg
            , cVerbose = verbose arg
            , cInliningSize = toThreshold $ inliningSize arg
            , cRecInliningSize = toThreshold $ recInliningSize arg
            , cMaxInlining = maxInlining arg
            , cActivatedOptim = selectedOptim
            , cILimit = iLimit arg
            , cFLimit = fLimit arg
            , cANSI = ansiSupported
            , cEmitParsed = emitAll arg || emitParsed arg
            , cEmitResolved = emitAll arg || emitResolved arg
            , cEmitTyped = emitAll arg || emitTyped arg
            , cEmitKNorm = emitAll arg || emitKNorm arg
            , cEmitFlatten = emitAll arg || emitFlatten arg
            , cEmitOptim = emitAll arg || emitOptim arg
            , cEmitClosure = emitAll arg || emitClosure arg
            , cEmitIR = emitAll arg || emitIR arg
            }
  where
    insertIf :: (Ord a) => Bool -> a -> Set a -> Set a
    insertIf True x = insert x
    insertIf False _ = id

    selectedOptim =
        insertIf (optimize arg || optCompMerging arg) CompMerging
            . insertIf (optimize arg || optConstFold arg) ConstFold
            . insertIf (optimize arg || optInlining arg) Inlining
            . insertIf (optimize arg || optUnusedElim arg) UnusedElim
            $ mempty

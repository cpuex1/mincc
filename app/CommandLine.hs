module CommandLine (
    ConfigIO,
    IdentEnvIO,
    BackendIdentStateIO,
    CompilerConfig (..),
    CommandLineArg (CommandLineArg),
    parseArg,
    toCompilerConfig,
) where

import BackEnd.Lowering (BackendIdentState)
import BackEnd.Optim (BackEndOptimKind (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Reader
import Data.Set (Set, insert)
import Error (CompilerError)
import MiddleEnd.Analysis.Identifier (IdentEnvT)
import MiddleEnd.Optim (OptimKind (..))
import MiddleEnd.Optim.Common (Threshold, toThreshold)
import Options.Applicative
import Registers (RegMultiple, RegTuple (createRT))
import System.Console.ANSI (hNowSupportsANSI)
import System.IO (stdout)

type ConfigIO = ReaderT CompilerConfig (ExceptT CompilerError IO)
type IdentEnvIO = IdentEnvT ConfigIO
type BackendIdentStateIO = BackendIdentState ConfigIO

defaultMaxInlining :: Int
defaultMaxInlining = 0

data CompilerConfig = CompilerConfig
    { cInput :: [String]
    , cOutput :: String
    , cVerbose :: Bool
    , cInliningSize :: Threshold
    , cRecInliningSize :: Threshold
    , cMaxInlining :: Int
    , cActivatedOptim :: Set OptimKind
    , cActivatedBackEndOptim :: Set BackEndOptimKind
    , cRegLimit :: RegMultiple Int
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
    , inliningSize :: Int
    , recInliningSize :: Int
    , maxInlining :: Int
    , disableFloatFold :: Bool
    , fusion :: Bool
    , optBoolOp :: Bool
    , optStripCondition :: Bool
    , optSwapIf :: Bool
    , optLoopArgsElim :: Bool
    , optReadOnly :: Bool
    , optCompMerging :: Bool
    , optIfMerging :: Bool
    , optCSE :: Bool
    , optVarMerging :: Bool
    , optConstFold :: Bool
    , optInlining :: Bool
    , optUnusedElim :: Bool
    , optLoopDetection :: Bool
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
                <> value (-1)
                <> metavar "THRESHOLD"
            )
        <*> option
            auto
            ( long "rec-inlining-size"
                <> help "The maximum size of recursive functions to be inlined. Infinity if not specified."
                <> value (-1)
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
            ( long "disable-float-fold"
                <> help "Disable constant folding for floating-point numbers"
            )
        <*> switch
            ( long "fusion"
                <> help "Enable instruction fusion"
            )
        <*> switch
            ( long "opt-bool-op"
                <> help "Enable bool operations optimization"
            )
        <*> switch
            ( long "opt-strip-condition"
                <> help "Enable condition stripping optimization"
            )
        <*> switch
            ( long "opt-swap-if"
                <> help "Enable if expressions swapping optimization"
            )
        <*> switch
            ( long "opt-loop-args-elim"
                <> help "Enable loop arguments elimination"
            )
        <*> switch
            ( long "opt-read-only"
                <> help "Enable read-only arrays elimination"
            )
        <*> switch
            ( long "opt-comp-merging"
                <> help "Enable comparison merging optimization"
            )
        <*> switch
            ( long "opt-if-merging"
                <> help "Enable if merging optimization"
            )
        <*> switch
            ( long "opt-cse"
                <> help "Enable common subexpression elimination optimization"
            )
        <*> switch
            ( long "opt-var-merging"
                <> help "Enable variable merging optimization"
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
            ( long "opt-loop-detection"
                <> help "Enable loop detection optimization"
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
                <> value 22
                <> metavar "INT"
            )
        <*> option
            auto
            ( long "f-limit"
                <> help "The maximum number of float registers"
                <> showDefault
                <> value 25
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
            , cInliningSize = toThreshold $ if inliningSize' < 0 then Nothing else Just inliningSize'
            , cRecInliningSize = toThreshold $ if recInliningSize' < 0 then Nothing else Just recInliningSize'
            , cMaxInlining = maxInlining arg
            , cActivatedOptim = selectedOptim
            , cActivatedBackEndOptim = selectedBackEndOptim
            , cRegLimit = createRT (iLimit arg) (fLimit arg)
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
    inliningSize' = inliningSize arg
    recInliningSize' = recInliningSize arg

    insertIf :: (Ord a) => Bool -> a -> Set a -> Set a
    insertIf True x = insert x
    insertIf False _ = id

    selectedOptim =
        insertIf (optimize arg || optCompMerging arg) CompMerging
            . insertIf (optimize arg || optIfMerging arg) IfMerging
            . insertIf (optimize arg || optReadOnly arg) ReadOnly
            . insertIf (optimize arg || optCSE arg) CSE
            . insertIf (optimize arg || optVarMerging arg) VarMerging
            . insertIf (optimize arg || optConstFold arg) ConstFold
            . insertIf ((optimize arg || optConstFold arg) && not (disableFloatFold arg)) ConstFoldFloat
            . insertIf (optimize arg || optUnusedElim arg) UnusedElim
            . insertIf (optimize arg || optInlining arg) Inlining
            . insertIf (optimize arg || optLoopDetection arg) LoopDetection
            . insertIf (optimize arg || optLoopArgsElim arg) LoopArgsElim
            . insertIf (optimize arg || optSwapIf arg) SwapIf
            . insertIf (optStripCondition arg) StripCondition
            . insertIf (optimize arg || optBoolOp arg) BoolOperation
            $ mempty

    selectedBackEndOptim =
        insert MulElim
            . insert Unreachable
            . insert Merging
            . insert EmptyBlockMerging
            . insert RegMerging
            . insert UnusedReg
            . insert UseZeroReg
            . insert ArgsRegReplacement
            . insert CloneRet
            . insertIf (fusion arg) Fusion
            $ mempty

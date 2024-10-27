module CommandLine (
    ConfigIO,
    CompilerConfig (CompilerConfig),
    CommandLineArg (CommandLineArg),
    cInput,
    cOutput,
    cIntermediate,
    cVerbose,
    cOptimize,
    cANSI,
    input,
    output,
    intermediate,
    verbose,
    optimize,
    parseArg,
    toCompilerConfig,
) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Reader
import Error (CompilerError)
import Options.Applicative
import System.Console.ANSI (hNowSupportsANSI)
import System.IO (stdout)

type ConfigIO = ReaderT CompilerConfig (ExceptT CompilerError IO)

data CompilerConfig = CompilerConfig
    { cInput :: [String]
    , cOutput :: String
    , cIntermediate :: Bool
    , cVerbose :: Bool
    , cOptimize :: Int
    , cANSI :: Bool
    }

data CommandLineArg = CommandLineArg
    { input :: [String]
    , output :: String
    , intermediate :: Bool
    , verbose :: Bool
    , optimize :: Int
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
            ( long "intermediate"
                <> help "Export intermediate files"
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
                <> help "Whether to be verbose"
            )
        <*> option
            auto
            ( long "optimize"
                <> help "Optimization level"
                <> showDefault
                <> value 1000
                <> metavar "INT"
            )

toCompilerConfig :: CommandLineArg -> IO CompilerConfig
toCompilerConfig (CommandLineArg i o inter v opt) = do
    ansiSupported <- hNowSupportsANSI stdout
    return $ CompilerConfig i o inter v opt ansiSupported

module CommandLine (ArgIO, CommandLineArg (CommandLineArg), input, output, intermediate, verbose, optimize, parseArg) where

import Options.Applicative
import Control.Monad.Trans.Reader

type ArgIO a = ReaderT CommandLineArg IO a

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

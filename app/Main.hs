{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative

data CommandLineArg = CommandLineArg
    { input :: [String]
    , output :: String
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

main :: IO ()
main = execParser opts >>= execArgs
  where
    opts =
        info
            (parseArg <**> helper)
            ( fullDesc
                <> progDesc "MinCaml ---[Mincc]---> RINANA"
                <> header "mincc (c) 2024 Akihisa Yagi\n\n"
            )

execArgs :: CommandLineArg -> IO ()
execArgs arg = print $ input arg

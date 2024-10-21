{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import Data.Text
import Data.Text.IO as TIO
import Parser (parseExpr)
import Syntax (Expr)
import Text.Megaparsec (parse, ParseErrorBundle (bundlePosState, bundleErrors), PosState (pstateSourcePos), errorBundlePretty, parseErrorTextPretty, parseErrorPretty)

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
execArgs arg = do
    args <- mapM compile $ input arg
    return ()

compile :: FilePath -> IO (Maybe Expr)
compile path = do
    content <- TIO.readFile path
    case parse parseExpr path content of
        Left err -> do
            let errs = bundleErrors err
            mapM_ (Prelude.putStrLn . parseErrorPretty) errs
            Prelude.putStrLn "error"
            return Nothing
        Right expr ->
            return (Just expr)

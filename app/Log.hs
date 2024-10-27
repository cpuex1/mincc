{-# LANGUAGE OverloadedStrings #-}

module Log (printLog, printTextLog, LogLevel (Debug, Info, Warn, Error, Fatal, Done)) where

import CommandLine
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO

import System.Console.ANSI

data LogLevel = Debug | Info | Warn | Error | Fatal | Done
    deriving (Eq, Ord, Show)

printLevel :: LogLevel -> String
printLevel Debug = "DEBUG"
printLevel Info = " INFO"
printLevel Warn = " WARN"
printLevel Error = "ERROR"
printLevel Fatal = "FATAL"
printLevel Done = " DONE"

setColor :: LogLevel -> IO ()
setColor Debug = setSGR [SetColor Foreground Vivid Black]
setColor Info = pure ()
setColor Warn = setSGR [SetColor Foreground Vivid Yellow]
setColor Error = setSGR [SetColor Foreground Vivid Red]
setColor Fatal = setSGR [SetColor Foreground Vivid Red]
setColor Done = setSGR [SetColor Foreground Vivid Green]

printLogGeneral :: (LogLevel -> Bool -> a -> IO ()) -> LogLevel -> a -> ConfigIO ()
printLogGeneral printFunc level message =
    ask >>= \config ->
        when (cVerbose config || level > Debug) $
            lift $
                printFunc level (cANSI config) message

printLog :: LogLevel -> String -> ConfigIO ()
printLog =
    printLogGeneral
        ( \level ansi message -> do
            if not ansi
                then
                    putStrLn $ "[" ++ printLevel level ++ "] " ++ message
                else
                    if level == Debug
                        then do
                            setColor Debug
                            putStrLn $ "[" ++ printLevel level ++ "] " ++ message
                            setSGR [Reset]
                        else do
                            setColor level
                            putStr $ "[" ++ printLevel level ++ "] "
                            setSGR [Reset]
                            putStrLn message
        )

printTextLog :: LogLevel -> Text -> ConfigIO ()
printTextLog =
    printLogGeneral
        ( \level ansi message -> do
            if not ansi
                then
                    TIO.putStrLn $ "[" <> pack (printLevel level) <> "] " <> message
                else
                    if level == Debug
                        then do
                            setColor Debug
                            TIO.putStrLn $ "[" <> pack (printLevel level) <> "] " <> message
                            setSGR [Reset]
                        else do
                            setColor level
                            TIO.putStr $ "[" <> pack (printLevel level) <> "] "
                            setSGR [Reset]
                            TIO.putStrLn message
        )

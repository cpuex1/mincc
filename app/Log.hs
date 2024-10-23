{-# LANGUAGE OverloadedStrings #-}

module Log (printLog, printTextLog, LogLevel (Debug, Info, Warn, Error, Fatal)) where

import CommandLine
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO

data LogLevel = Debug | Info | Warn | Error | Fatal
    deriving (Eq, Ord, Show)

printLevel :: LogLevel -> String
printLevel Debug = "DEBUG"
printLevel Info = " INFO"
printLevel Warn = " WARN"
printLevel Error = "ERROR"
printLevel Fatal = "FATAL"

printLogGeneral :: (LogLevel -> a -> IO ()) -> LogLevel -> a -> ArgIO ()
printLogGeneral printFunc level message =
    ask >>= \config ->
        when (verbose config || level > Debug) $
            lift $
                printFunc level message

printLog :: LogLevel -> String -> ArgIO ()
printLog = printLogGeneral (\level message -> putStrLn $ "[" ++ printLevel level ++ "] " ++ message)

printTextLog :: LogLevel -> Text -> ArgIO ()
printTextLog = printLogGeneral (\level message -> TIO.putStrLn $ "[" <> pack (printLevel level) <> "] " <> message)

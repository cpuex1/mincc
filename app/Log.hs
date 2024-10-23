module Log (printLog, LogLevel(Debug, Info, Warn, Error, Fatal)) where

import CommandLine (CommandLineArg, verbose)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

data LogLevel = Debug | Info | Warn | Error | Fatal
    deriving (Eq, Ord, Show)

printLog :: LogLevel -> String -> ReaderT CommandLineArg IO ()
printLog level message =
    ask >>= \config ->
        when (verbose config || level > Debug) $
            lift $
                putStrLn $
                    "[" ++ printLevel level ++ "] " ++ message
  where
    printLevel :: LogLevel -> String
    printLevel Debug = "DEBUG"
    printLevel Info = " INFO"
    printLevel Warn = " WARN"
    printLevel Error = "ERROR"
    printLevel Fatal = "FATAL"

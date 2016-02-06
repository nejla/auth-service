-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
module Logging where

import           Control.Monad.Trans
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.IO as Text
import           System.IO
import           Types

data LogLevel = Debug | Info | Warn | Error deriving (Eq, Show)

logLevelToText :: LogLevel -> Text
logLevelToText Debug  = "DEBUG"
logLevelToText Info  = "INFO"
logLevelToText Warn  = "WARN"
logLevelToText Error = "ERROR"

logMessage :: LogLevel -> Text -> API ()
logMessage level msg = do
    liftIO . Text.hPutStrLn stderr $ Text.concat [ "[", logLevelToText level , "] "
                                                 ,  msg
                                                 ]

logDebug, logInfo, logWarn, logError :: Text -> API ()
logDebug =  logMessage Debug
logInfo = logMessage Info
logWarn = logMessage Warn
logError = logMessage Error

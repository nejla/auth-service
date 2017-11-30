-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Config
  ( module NejlaCommon.Config
  , module Config
  ) where

import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Configurator.Types as Conf
import qualified System.Exit as Exit

import           Types

import           NejlaCommon.Config hiding (Config)

--------------------------------------------------------------------------------
-- Configuration ---------------------------------------------------------------
--------------------------------------------------------------------------------

getTwilioConfig :: (MonadIO m, MonadLogger m) =>
                   Conf.Config
                -> m (Maybe TwilioConfig)
getTwilioConfig conf = do
    mbAccount <- getConfMaybe "TWILIO_ACCOUNT" "twilio.account" conf
    mbAuthToken <- getConfMaybe "TWILIO_TOKEN" "twilio.token" conf
    mbSourceNumber <- getConfMaybe "TWILIO_SOURCE" "twilio.source" conf
    case (mbAccount, mbAuthToken, mbSourceNumber) of
        (Nothing, Nothing, Nothing) -> return Nothing
        (Just acc, Just authT, Just sourceNo) ->
            return $ Just TwilioConfig { twilioConfigAccount = acc
                                       , twilioConfigAuthToken = authT
                                       , twilioConfigSourceNumber = sourceNo
                                       }
        _ -> do
            $logError "Twilio config is incomplete"
            liftIO Exit.exitFailure

get2FAConf :: (MonadLogger m, MonadIO m) =>
              Conf.Config
           -> m (Bool, Maybe TwilioConfig)
get2FAConf conf = do
     tfaRequired <- getConfBool "TFA_REQUIRED" "tfa.required" (Right False) conf
     twilioConf <- getTwilioConfig conf
     case (tfaRequired, twilioConf) of
      (True, Nothing) -> do
          $logError "Two Factor Authentication is required, but Twilio is not configured"
          liftIO Exit.exitFailure
      _ -> return (tfaRequired, twilioConf)


getAuthServiceConfig :: (MonadIO m, MonadLogger m) =>
                        Conf.Config
                     -> m Config
getAuthServiceConfig conf = do
    to <- getConf' "TOKEN_TIMEOUT" "token.timeout"
                 (Right 3600) {- 1 hour -} conf
    otpl <- getConf' "OTP_LENGTH" "otp.length"
                 (Right 4) conf
    otpt <- getConf' "OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    (tfaRequired, twilioConf) <- get2FAConf conf
    return Config{ configTimeout = to
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configTwilio = twilioConf
                 , configUseTransactionLevels = True
                 }

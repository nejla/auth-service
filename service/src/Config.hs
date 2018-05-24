{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All Rights Reserved

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Config
  ( module NejlaCommon.Config
  , module Config
  ) where

import           Control.Lens            ((^.))
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Aeson              as Aeson
import qualified Data.Configurator.Types as Conf
import           Data.Default            (def)
import           Data.Monoid
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.IO       as LText
import qualified Network.Mail.Mime       as Mail
import qualified System.Exit             as Exit
import           System.IO               (stderr, hFlush)
import qualified Text.Microstache        as Mustache
import qualified Twilio

import           Types
import           Util

import           NejlaCommon.Config      hiding (Config)

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
    haveEmail <- setEmailConf conf
    let configOtp = fmap Twilio.sendMessage twilioConf
    return Config{ configTimeout = to
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configOtp = configOtp
                 , configUseTransactionLevels = True
                 , configEmail = haveEmail
                 }

-- Default template loaded from src/password-reset-email-template.html.mustache
defaultPwResetTemplate :: Mustache.Template
defaultPwResetTemplate =
  $(htmlTemplate "password-reset-email-template.html.mustache")

defaultPwResetUnknownTemplate :: Mustache.Template
defaultPwResetUnknownTemplate =
  $(htmlTemplate "password-reset-unknown-email-template.html.mustache")


setEmailConf :: (MonadIO m, MonadLogger m) => Conf.Config -> m (Maybe EmailConfig)
setEmailConf conf =
  getConfMaybe "EMAIL_FROM" "email.from" conf >>= \case
    Nothing -> return Nothing
    Just fromAddress -> do
      fromName <- getConfMaybe "EMAIL_FROM_NAME" "email.fromName" conf
      let emailConfigFrom = Mail.Address fromName fromAddress
      emailConfigHost <-
        getConf "EMAIL_SMTP" "email.smtp" (Left "email host") conf
      emailConfigPort <-
        getConf' "EMAIL_PORT" "email.port" (Right 25) conf
      emailConfigUser <-
        getConf "EMAIL_USER" "email.user" (Left "email user") conf
      emailConfigPassword <-
        getConf "EMAIL_PASSWORD" "email.password" (Left "email password") conf
      emailConfigSiteName <-
        getConf "SITE_NAME" "site-name" (Left "site name") conf
      emailConfigResetLinkExpirationTime <-
        getConf
          "RESET_LINK_EXPIRATION_TIME"
          "email.link-expiration-time"
          (Left "Human-readable reset link expiration time")
          conf
      mbEmailConfigTemplatefile <-
        getConfMaybe "EMAIL_TEMPLATE" "email.template" conf
      emailConfigPWResetTemplate <-
        case mbEmailConfigTemplatefile of
          Nothing -> return defaultPwResetTemplate
          Just filename ->
            liftIO . Mustache.compileMustacheFile $ Text.unpack filename
      mbEmailConfigUnknownTemplatefile <-
        getConfMaybe "EMAIL_UNKNOWN_TEMPLATE" "email.unknown-template" conf
      emailConfigPWResetUnknownTemplate <-
        case mbEmailConfigUnknownTemplatefile of
          Nothing -> return defaultPwResetUnknownTemplate
          Just filename ->
            liftIO . Mustache.compileMustacheFile $ Text.unpack filename
      emailConfigLinkTemplate <-
        getConf
          "EMAIL_LINK_TEMPLATE"
          "email.link-template"
          (Left "Password reset email link template")
          conf
      let emailConfigMkLink =
            \tok -> Text.replace "%s" tok emailConfigLinkTemplate
      sendmailCommand <-
        getConfMaybe "SENDMAIL_PROGRAM" "email.sendmail-program" conf
      emailConfigTls <- getConfBool "EMAIL_TLS" "email.tls" (Right True) conf
      emailConfigAuth <- getConfBool "EMAIL_AUTH" "email.auth" (Right True) conf
      let emailConfigSendmail =
            case sendmailCommand of
              Just cmd
                | (prg:args) <- Text.splitOn " " cmd
                , not (Text.null prg) ->
                  SendmailConfig
                  { sendmailConfigPath = Text.unpack $ prg
                  , sendmailConfigArguments = Text.unpack <$> args
                  }
              _ -> def
          ecfg = EmailConfig {..}
      writeMsmtprc ecfg emailConfigTls emailConfigAuth
      return $ Just ecfg

writeMsmtprc ::
     (MonadLogger m, MonadIO m) => EmailConfig -> Bool -> Bool -> m ()
writeMsmtprc emailConfig tls auth = do
  txt <- renderMsmtprc emailConfig tls auth
  liftIO $ LText.hPutStrLn stderr txt
  liftIO $ hFlush stderr
  liftIO $ LText.writeFile "/etc/msmtprc" txt

renderMsmtprc ::
     (MonadIO m, MonadLogger m)
  => EmailConfig
  -> Bool
  -> Bool
  -> m LText.Text
renderMsmtprc cfg tls auth =
  let Mail.Address _ fromAddress = cfg ^. from
      dt = Aeson.object [ "host" .= (cfg ^. host)
                        , "port" .= (cfg ^. port)
                        , "fromAddress" .= fromAddress
                        , "password" .= cfg ^. password
                        , "tls" .= tls
                        , "auth" .= auth
                        , "user" .= cfg ^. user
                        ]
  in case Mustache.renderMustacheW template dt of
       ([], txt) -> return txt
       (warnings, _) -> do
         $logError $ "Could not render msmtprc: " <> (Text.pack $ show warnings)
         liftIO Exit.exitFailure
  where
    infix 0 .=
    (.=) = (Aeson..=)
    template = [mustache|
defaults

{{#tls}}
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt
{{/tls}}
{{^tls}}
tls off
{{/tls}}

timeout 30

account defaultaccount
host {{host}}
port {{port}}
from {{fromAddress}}
{{#auth}}
auth on
user {{user}}
password {{password}}
{{/auth}}
{{^auth}}
auth off
{{/auth}}

account default : defaultaccount
|]

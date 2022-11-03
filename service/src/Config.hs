{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( module NejlaCommon.Config
  , module Config
  ) where

import           Control.Lens             ((^.))
import           Control.Monad            (when)
import qualified Control.Monad.Catch      as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.Char                as Char
import qualified Data.Configurator.Types  as Conf
import           Data.Default             (def)
import           Data.Maybe               (maybeToList)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy           as LText
import qualified Data.Text.Lazy.IO        as LText
import           Data.UUID                (UUID)
import qualified Data.UUID                as UUID
import qualified Network.Mail.Mime        as Mail
import qualified SignedAuth
import           System.Exit              (exitFailure)
import qualified System.Exit              as Exit
import           System.IO                (hPutStrLn, stderr)
import           System.IO.Error          (isDoesNotExistError, isPermissionError)
import qualified System.Posix.Files       as Posix
import qualified Text.Microstache         as Mustache
import qualified Twilio

import           Types
import           Util

import           NejlaCommon.Config       hiding (Config)
import           SAML.Config

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

instance Conf.Configured UUID where
  convert (Conf.String txt) = UUID.fromText txt
  convert _ = Nothing

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


getAccountCreationConfig ::
     (MonadLogger m, MonadIO m) => Conf.Config -> m AccountCreationConfig
getAccountCreationConfig conf = do
  accountCreationConfigEnabled <-
    getConfBool "ACCOUNT_CREATION" "account_creation.enabled" (Right False) conf
  accountCreationConfigDefaultInstances <-
    fmap InstanceID . maybeToList <$>
    getConfGenericMaybe
      (UUID.fromText . Text.pack)
      "DEFAULT_INSTANCE"
      "default-instance"
      conf
  return AccountCreationConfig{..}

readSignedHeaderKey :: MonadIO m => FilePath -> m SignedAuth.PrivateKey
readSignedHeaderKey path = liftIO $ do
  isPipe <- Posix.isNamedPipe <$> Posix.getFileStatus path
  keyBS <- Ex.catch (stripEnd <$> BS.readFile path)
    ( \(e :: IOError) -> do
      if
        | isDoesNotExistError e  ->
            hPutStrLn stderr
            $ path ++ " does not exist. Please create it before starting or set\
                      \ SIGNED_HEADERS_PRIVATE_KEY_PATH to the correct path"
        | isPermissionError e ->
            hPutStrLn stderr
            $ "Permission error reading " ++ path ++ ". Please check permissions\
              \ or set SIGNED_HEADERS_PRIVATE_KEY_PATH to the correct path"
        | otherwise ->
            hPutStrLn stderr
            $ path ++ " could not be read. Encountered error: " ++ show e
            ++ ". Check if SIGNED_HEADERS_PRIVATE_KEY_PATH is set to the correct path"
      exitFailure)
  when (BS.null keyBS && isPipe) $ do
    hPutStrLn stderr $ path ++ " is a named pipe and is empty.\
                               \ Did you forget to write the secret to it?"
    exitFailure

  case SignedAuth.readPrivateKeyDer keyBS of
    Left e -> liftIO $ do
      hPutStrLn stderr $ "Could not parse DER-encoded key: \""
        ++ Text.unpack (Text.decodeUtf8With lenientDecode keyBS)
        ++ "\" reason: " ++ e
      exitFailure
    Right r -> return r
  where
    stripEnd bs =
      let (bs', _end) = BS8.spanEnd Char.isSpace bs
      in bs'



getAuthServiceConfig :: (MonadIO m, MonadLogger m) =>
                        Conf.Config
                     -> m Config
getAuthServiceConfig conf = do
    to <- getConfMaybe' "TOKEN_TIMEOUT" "token.timeout" conf
    tuto <- getConfMaybe' "TOKEN_UNUSED_TIMEOUT" "token.unused-timeout" conf

    configMaxAttempts <- getConf' "MAX_LOGIN_ATTEMPTS" "max-login-attempts"
                           (Right 5) conf
    configAttemptsTimeframe <- fromInteger <$> getConf'
                                 "LOGIN_RATE_TIMEFRAME_SECONDS"
                                 "login-attempts-timeframe-seconds"
                                 (Right 60) conf
    otpl <- getConf' "OTP_LENGTH" "otp.length"
                 (Right 4) conf
    otpt <- getConf' "OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    (tfaRequired, twilioConf) <- get2FAConf conf
    haveEmail <- setEmailConf conf
    let configOtp = fmap Twilio.sendMessage twilioConf
    accountCreationConfig <- getAccountCreationConfig conf

    samlConfig <- getSAMLConfig conf

    return Config{ configTimeout = to
                 , configTokenUnusedTimeout = tuto
                 , configMaxAttempts = configMaxAttempts
                 , configAttemptsTimeframe = configAttemptsTimeframe
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configOtp = configOtp
                 , configUseTransactionLevels = True
                 , configEmail = haveEmail
                 , configAccountCreation = accountCreationConfig
                 , configSamlConfig = samlConfig
                 }

getSecrets :: (MonadIO m, MonadLogger m) => Conf.Config -> m Secrets
getSecrets conf = do
  signedHeaderKeyPath <-
    getConf "SIGNED_HEADERS_PRIVATE_KEY_PATH" "signed-headers.private-key-path"
      (Right "/run/secrets/header_signing_private_key") conf
  signedHeaderKey <- readSignedHeaderKey $ Text.unpack signedHeaderKeyPath
  serviceToken <-
    getConf "SERVICE_TOKEN" "service-token"
      (Left "Secret token for between-service communication") conf
  return Secrets { secretsHeaderPrivateKey = signedHeaderKey
                 , secretsServiceToken = serviceToken
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
        getConf'
          "RESET_LINK_EXPIRATION_TIME"
          "email.link-expiration-time"
          (Right 24)
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
                  { sendmailConfigPath = Text.unpack prg
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
         $logError $ "Could not render msmtprc: " <> Text.pack (show warnings)
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

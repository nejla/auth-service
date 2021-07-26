{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module AuthService.Types
  , module Types
  ) where

import           Control.Lens
import qualified Control.Monad.Catch  as Ex
import           Data.Default
import           Data.Text            (Text)
import           Data.Time            (NominalDiffTime)
import           Data.Typeable
import           Network.Mail.Mime    (Address)
import qualified Text.Microstache     as Mustache

import qualified SignedAuth
import           AuthService.Types
import           Control.Monad.Logger (LoggingT)

--------------------------------------------------------------------------------
-- Error
--------------------------------------------------------------------------------

data EmailError = EmailErrorNotConfigured
                | EmailRenderError
                | EmailAddressUnknownError
                deriving (Show, Eq, Typeable)

instance Ex.Exception EmailError

data LoginError = LoginErrorFailed -- Username not found, password wrong or OTP
                                   -- wrong
                | LoginErrorOTPRequired
                | LoginErrorTwilioNotConfigured
                | LoginErrorRatelimit
                  deriving (Show, Eq)

makePrisms ''LoginError

data ChangePasswordError = ChangePasswordLoginError LoginError
                         | ChangePasswordTokenError
                         | ChangePasswordHashError
                         | ChangePasswordUserDoesNotExistError
                           deriving (Show, Eq, Typeable)

instance Ex.Exception ChangePasswordError

makePrisms ''ChangePasswordError

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------
type PwResetToken = Text

data TwilioConfig = TwilioConfig
  { twilioConfigAccount      :: Text
  , twilioConfigAuthToken    :: Text
  , twilioConfigSourceNumber :: Text
  } deriving (Show)


data SendmailConfig =
  SendmailConfig
  { sendmailConfigPath :: String
  , sendmailConfigArguments :: [String]
  } deriving Show

instance Default SendmailConfig where
  def =
    SendmailConfig
    { sendmailConfigPath = "/usr/sbin/sendmail"
    , sendmailConfigArguments = ["-t"]
    }

data EmailConfig = EmailConfig
  { emailConfigHost :: Text
  , emailConfigPort :: Int
  , emailConfigFrom :: Address
  , emailConfigUser :: Text
  , emailConfigPassword :: Text
  , emailConfigPWResetTemplate :: Mustache.Template
  , emailConfigPWResetUnknownTemplate :: Mustache.Template
  , emailConfigSendmail :: SendmailConfig
  , emailConfigSiteName :: Text
  , emailConfigResetLinkExpirationTime :: Int -- ^ Time in hours
  , emailConfigMkLink :: PwResetToken -> Text -- ^ Generate a link from a token
  }

type OtpHandler = Phone -> Text -> LoggingT IO ()

data AccountCreationConfig =
  AccountCreationConfig
  { accountCreationConfigEnabled :: Bool
  , accountCreationConfigDefaultInstances :: [InstanceID]
  }

data Config = Config
  { configTimeout              :: Maybe Integer -- token timeout in seconds
  , configTokenUnusedTimeout   :: Maybe Integer
  , configMaxAttempts          :: Integer -- Number of password attempts per
                                          -- time frame (e.g. 5 in the last minute)
  , configAttemptsTimeframe    :: NominalDiffTime
                                  -- Time frame in which login attempts are
                                  -- counted (e.g. 60 for one minute)
  , configOTPLength            :: Int
  , configOTPTimeoutSeconds    :: Integer
  , configTFARequired          :: Bool
  , configOtp                  :: Maybe OtpHandler
  , configUseTransactionLevels :: Bool
  , configEmail                :: Maybe EmailConfig

  , configAccountCreation      :: AccountCreationConfig
  }

newtype Secrets =
  Secrets
    { secretsHeaderPrivateKey     :: SignedAuth.PrivateKey
    }

-- | Necessary data to fill in a password reset email
data EmailData =
  EmailData
  { emailDataSiteName       :: Text
  , emailDataLink           :: Text
  , emailDataExpirationTime :: Text
  } deriving (Show)

makeLensesWith camelCaseFields ''EmailData
makeLensesWith camelCaseFields ''AccountCreationConfig
makeLensesWith camelCaseFields ''Config
makeLensesWith camelCaseFields ''Secrets
makeLensesWith camelCaseFields ''EmailConfig
makeLensesWith camelCaseFields ''SendmailConfig
makeLensesWith camelCaseFields ''TwilioConfig

-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Monad.Reader
import           Data.Default
import           Data.Text            (Text)
import           Data.Typeable
import           Database.Persist.Sql
import qualified NejlaCommon          as NC
import           Network.Mail.Mime    (Address)
import qualified Text.Microstache     as Mustache

import           AuthService.Types

--------------------------------------------------------------------------------
-- Error -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data EmailError = EmailErrorNotConfigured
                | EmailRenderError
                | EmailAddressUnknownError
                deriving (Show, Eq, Typeable)

instance Ex.Exception EmailError

data LoginError = LoginErrorFailed -- Username not found, password wrong or OTP
                                   -- wrong
                | LoginErrorOTPRequired
                | LoginTwilioNotConfigured
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
-- Config ----------------------------------------------------------------------
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
  , emailConfigResetLinkExpirationTime :: Text
  , emailConfigMkLink :: PwResetToken -> Text -- ^ Generate a link from a token
  }

type OtpHandler = Phone -> Text -> API ()

data Config = Config
  { configTimeout              :: Integer -- token timeout in seconds
  , configOTPLength            :: Int
  , configOTPTimeoutSeconds    :: Integer
  , configTFARequired          :: Bool
  , configOtp                  :: Maybe OtpHandler
  , configUseTransactionLevels :: Bool
  , configEmail                :: Maybe EmailConfig
  }

-- | Necessary data to fill in a password reset email
data EmailData =
  EmailData
  { emailDataSiteName       :: Text
  , emailDataLink           :: Text
  , emailDataExpirationTime :: Text
  } deriving (Show)

--------------------------------------------------------------------------------
-- Monad -----------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ApiState = ApiState { apiStateConfig :: Config
                            }

type API a = NC.App ApiState 'NC.Privileged 'NC.ReadCommitted a

-- newtype API a = API { unAPI :: ReaderT ApiState IO a }
--               deriving ( Functor, Applicative, Monad, MonadIO
--                        , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB = NC.db'

makeLensesWith camelCaseFields ''ApiState
makeLensesWith camelCaseFields ''EmailData
makeLensesWith camelCaseFields ''Config
makeLensesWith camelCaseFields ''EmailConfig
makeLensesWith camelCaseFields ''SendmailConfig
makeLensesWith camelCaseFields ''TwilioConfig

getConfig ::  Lens' Config a -> API a
getConfig g = NC.viewState $ config . g

runAPI :: ConnectionPool -> Config -> API a -> IO a
runAPI pool conf m =
  let st = ApiState { apiStateConfig = conf }
  in NC.runApp' (def & NC.useTransactionLevels .~ (conf ^. useTransactionLevels))
                 pool st m

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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module AuthService.Types
  , module Types
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default
import           Data.Text (Text)
import           Database.Persist.Sql
import qualified NejlaCommon as NC

import           AuthService.Types

--------------------------------------------------------------------------------
-- Error -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data LoginError = LoginErrorFailed -- Username not found, password wrong or OTP
                                   -- wrong
                | LoginErrorOTPRequired
                | LoginTwilioNotConfigured
                  deriving (Show, Eq)

makePrisms ''LoginError

data ChangePasswordError = ChangePasswordLoginError LoginError
                         | ChangePasswordTokenError
                         | ChangePasswordHashError
                           deriving (Show, Eq)

makePrisms ''ChangePasswordError

--------------------------------------------------------------------------------
-- Config ----------------------------------------------------------------------
--------------------------------------------------------------------------------


data TwilioConfig = TwilioConfig { twilioConfigAccount :: !Text
                                 , twilioConfigAuthToken :: !Text
                                 , twilioConfigSourceNumber :: !Text
                                 } deriving Show

makeLensesWith camelCaseFields ''TwilioConfig

data Config = Config { configTimeout :: !Integer -- token timeout in seconds
                     , configOTPLength :: !Int
                     , configOTPTimeoutSeconds :: !Integer
                     , configTFARequired :: !Bool
                     , configTwilio :: !(Maybe TwilioConfig)
                     , configUseTransactionLevels :: !Bool
                     } deriving Show

makeLensesWith camelCaseFields ''Config

--------------------------------------------------------------------------------
-- Monad -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data ApiState = ApiState { apiStateConfig :: Config
                         }

makeLensesWith camelCaseFields ''ApiState

type API a = NC.App ApiState 'NC.Privileged 'NC.ReadCommitted a

-- newtype API a = API { unAPI :: ReaderT ApiState IO a }
--               deriving ( Functor, Applicative, Monad, MonadIO
--                        , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB m = NC.db' m

getConfig ::  Lens' Config a -> API a
getConfig g = NC.viewState $ config . g

runAPI :: ConnectionPool -> Config -> API a -> IO a
runAPI pool conf m =
  let st = ApiState { apiStateConfig = conf }
  in NC.runApp' (def & NC.useTransactionLevels .~ (conf ^. useTransactionLevels))
                 pool st m

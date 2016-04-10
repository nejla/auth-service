-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module AuthServiceTypes
  , module Types
  ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.Persist.Sql
import           Servant
import           Web.HttpApiData

import           AuthServiceTypes

--------------------------------------------------------------------------------
-- Error -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data LoginError = LoginErrorFailed -- Username not found, password wrong or OTP
                                   -- wrong
                | LoginErrorOTPRequired
                | LoginTwilioNotConfigured
                  deriving (Show, Eq)

makePrisms ''LoginError

--------------------------------------------------------------------------------
-- Config ----------------------------------------------------------------------
--------------------------------------------------------------------------------


data TwilioConfig = TwilioConfig { twilioConfigAccount :: !Text
                                 , twilioConfigAuthToken :: !Text
                                 , twilioConfigSourceNumber :: !Text
                                 } deriving Show

makeLensesWith camelCaseFields ''TwilioConfig

data Config = Config { configTimeout :: !Integer -- token timeout in seconds
                     , configDbString :: !ByteString
                     , configOTPLength :: !Int
                     , configOTPTimeoutSeconds :: !Integer
                     , configTFARequired :: !Bool
                     , configTwilio :: !(Maybe TwilioConfig)
                     } deriving Show

makeLensesWith camelCaseFields ''Config

--------------------------------------------------------------------------------
-- Monad -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data ApiState = ApiState { apiStateSqlCon :: SqlBackend
                         , apiStateConfig :: Config
                         }

makeLensesWith camelCaseFields ''ApiState

newtype API a = API { unAPI :: ReaderT ApiState IO a }
              deriving ( Functor, Applicative, Monad, MonadIO
                       , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB m = do
    con <- API $ view sqlCon
    liftIO $ runReaderT m con

getConfig ::  Lens' Config a -> API a
getConfig g = API . view $ config . g

runAPI :: ConnectionPool -> Config -> API a -> IO a
runAPI pool conf (API m) = flip runSqlPool pool . ReaderT $ \con ->
    let state = ApiState { apiStateSqlCon = con
                         , apiStateConfig = conf
                         }
    in runReaderT m state

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Common where

import           Control.Monad.Logger
import           Data.Maybe              (fromJust)
import           Data.Monoid
import           Data.Pool
import           Data.Time.Clock
import           Data.UUID               (UUID)
import qualified Data.UUID               as UUID
import           Database.Persist.Sqlite as SQLite
import           Test.Hspec.Wai
import qualified Text.Microstache        as Mustache

import qualified Persist.Schema          as DB
import           Types

withMemoryPool :: (Pool SqlBackend -> IO a) -> IO a
withMemoryPool f = runNoLoggingT . withSqliteConn ":memory:" $ \con -> liftIO $ do
  pool <- createPool (return con) (\_ -> return ()) 1 3600 1
  f pool

testEmailConfig :: EmailConfig
testEmailConfig =
  EmailConfig
  { emailConfigHost     = "localhost"
  , emailConfigPort     = 25
  , emailConfigFrom     = "testuser@localhost"
  , emailConfigUser     = "testuser"
  , emailConfigPassword = "pwd"
  , emailConfigPWResetTemplate = tmpl
  , emailConfigPWResetUnknownTemplate = tmpl2
  , emailConfigSendmail =
      SendmailConfig
      { sendmailConfigPath = "/usr/bin/cat"
      , sendmailConfigArguments = []
      }
  , emailConfigSiteName = "Test Site"
  , emailConfigResetLinkExpirationTime = 24
  , emailConfigMkLink = \tok -> "http://localhost/reset?token=" <> tok
  }
  where
    Right tmpl =
      Mustache.compileMustacheText
        "email template"
        "please click on {{link}}"
    Right tmpl2 =
      Mustache.compileMustacheText
        "email template"
        "Your email is unknown"


accountCreationConfig = AccountCreationConfig
  { accountCreationConfigEnabled = True
  , accountCreationConfigDefaultInstances =
              [ InstanceID . fromJust $
                UUID.fromText "de305d54-75b4-431b-adb2-eb6b9e546014"
              ]
  }

withApiData :: Maybe OtpHandler -> (Pool SqlBackend -> Config -> IO a) -> IO a
withApiData mbHandleOtp f =
  withMemoryPool $ \pool -> do
    let conf =
          Config
          { configTimeout = 9999
          , configOTPLength = 6
          , configOTPTimeoutSeconds = 10
          , configTFARequired = True
          , configOtp = mbHandleOtp
          , configUseTransactionLevels = False
          , configEmail = Just testEmailConfig
          , configAccountCreation = accountCreationConfig
          }
    liftIO $ do
      _ <- runSqlPool (runMigrationSilent DB.migrateAll) pool
      f pool conf

withRunAPI :: Maybe OtpHandler -> ((forall a. API a -> IO a) -> IO b) -> IO b
withRunAPI mbOtpHandler f = withApiData mbOtpHandler
                            $ \pool conf -> f $  runAPI pool conf

seconds :: Integer -> NominalDiffTime
seconds s = fromIntegral s

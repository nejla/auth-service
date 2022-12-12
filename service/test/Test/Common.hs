{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Common where

import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Maybe                        (fromJust)
import           Data.Pool
import           Data.Time.Clock
import qualified Data.UUID                         as UUID
import           Database.Persist.Postgresql       as Postgres
import           Database.Persist.Sql              as P
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax        as TH
import qualified NejlaCommon                       as NC
import qualified NejlaCommon.Config                as NC
import           NejlaCommon.Persistence.Migration (sql)
import qualified NejlaCommon.Test                  as NC
import qualified SignedAuth
import qualified Text.Microstache                  as Mustache

import           Persist.Migration                 (doMigrate)

import           Audit                             (AuditSource(AuditSourceTest))
import           Monad
import           Types

withPsqlPool :: (Pool SqlBackend -> NoLoggingT IO a) -> IO a
withPsqlPool f = runNoLoggingT $ do
  conf <- NC.loadConf "auth-service-test"
  ci <- NC.getDBConnectInfo conf
  NC.withDBPool ci 5 (return ()) f

testEmailConfig :: EmailConfig
testEmailConfig =
  EmailConfig
  { emailConfigHost     = "localhost"
  , emailConfigPort     = 25
  , emailConfigFrom     = "testuser@localhost"
  , emailConfigUser     = "testuser"
  , emailConfigPassword = "pwd"
  , emailConfigPWResetTemplate = tmpl "please click on http://localhost/reset?token={{token}}"
  , emailConfigPWResetUnknownTemplate = tmpl "Your email is unknown"
  , emailConfigSendmail =
      SendmailConfig
      { sendmailConfigPath = "/usr/bin/cat"
      , sendmailConfigArguments = []
      }
  , emailConfigResetLinkExpirationTime = 24
  }
  where
    tmpl x = case Mustache.compileMustacheText "email template" x of
               Right template -> template
               Left e -> error $ show e

accountCreationConfig :: AccountCreationConfig
accountCreationConfig = AccountCreationConfig
  { accountCreationConfigEnabled = True
  , accountCreationConfigDefaultInstances =
              [
              ]
  }

withTestDB :: (Pool SqlBackend -> IO a) -> IO a
withTestDB f = runNoLoggingT $ do
    conf <- NC.loadConf "auth-service-test"
    ci <- NC.getDBConnectInfo conf
    NC.withTestDB ci 5 doMigrate $ lift . f

type TestCase = Postgres.ConnectionPool -> IO ()

mkConfig :: ConnectionPool
         -> IO (Config, Secrets)
mkConfig pool = do
    runSqlPool cleanDB pool
    (privateKey, publicKey) <- SignedAuth.mkKeys
    let conf =
          Config
          { configTimeout = Nothing
          , configTokenUnusedTimeout = Nothing
          , configMaxAttempts = 10000
          , configAttemptsTimeframe  =1
          , configOTPLength = 6
          , configOTPTimeoutSeconds = 10
          , configTFARequired = True
          , configOtp = Nothing
          , configUseTransactionLevels = False
          , configEmail = Just testEmailConfig
          , configAccountCreation = accountCreationConfig
          , configSamlConfig = Nothing
          }
        secrets = Secrets { secretsHeaderPrivateKey = privateKey
                          , secretsServiceToken = Nothing
                          }
    return (conf, secrets)
  where
    -- | Delete all rows from all tables (Don't use TRUNACE TABLE since it's
    -- slower)
    cleanDB = NC.cleanDB


withRunAPI :: (Config -> Config)
           -> ConnectionPool
           -> ((forall a. API a -> IO a) -> IO b)
           -> IO b
withRunAPI changeConf pool f = do
  (conf, secrets) <- mkConfig pool
  noncePool <- SignedAuth.newNoncePool
  let apiState = ApiState { apiStateConfig = changeConf conf
                          , apiStateAuditSource = AuditSourceTest
                          , apiStateNoncePool = noncePool
                          }
  f $ runAPI pool apiState


seconds :: Integer -> NominalDiffTime
seconds s = fromIntegral s


uuidQ :: QuasiQuoter
uuidQ =
  QuasiQuoter
  { quoteExp = \txt ->
                 case UUID.fromString txt of
                     Nothing -> fail $ "Could not read UUID " ++ show txt
                     Just u -> TH.lift u
  , quoteDec = error "uuid.quoteDec: Not implemented"
  , quotePat = error "uuid.quotePat: Not implemented"
  , quoteType = error "uuid.quoteType: Not implemented"
  }

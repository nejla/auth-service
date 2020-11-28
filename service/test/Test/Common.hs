{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Common where

import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Data.Maybe                        (fromJust)
import           Data.Pool
import           Data.Time.Clock
import qualified Data.UUID                         as UUID
import           Database.Persist.Postgresql       as Postgres
import           Database.Persist.Sql              as P
import qualified NejlaCommon                       as NC
import qualified NejlaCommon.Config                as NC
import           NejlaCommon.Persistence.Migration (sql)
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


accountCreationConfig :: AccountCreationConfig
accountCreationConfig = AccountCreationConfig
  { accountCreationConfigEnabled = True
  , accountCreationConfigDefaultInstances =
              [
              ]
  }

withTestDB :: (Pool SqlBackend -> IO a) -> IO a
withTestDB f =
  withPsqlPool $ \pool -> do
    liftIO $ do
      _ <- runLoggingT (NC.runPoolRetry pool dbSetup) (\_ _ _ _ -> return ())
      f pool
  where
    dbSetup = do
      resetDB
      doMigrate
      makeConstraintsDeferrable
    resetDB = P.rawExecute
      [sql|
        SET client_min_messages TO ERROR;
        DROP SCHEMA IF EXISTS _meta CASCADE;
        DROP SCHEMA public CASCADE;
        CREATE SCHEMA public;
        GRANT ALL ON SCHEMA public TO postgres;
        GRANT ALL ON SCHEMA public TO public;
        COMMENT ON SCHEMA public IS 'standard public schema';
        RESET client_min_messages;
        |] []
    -- Iterate over all foreign constraints and make them deferrable (so we can
    -- DELETE them without having to worry about the order we do it in)
    makeConstraintsDeferrable = P.rawExecute
      [sql|
          DO $$
            DECLARE
                statements CURSOR FOR
                    SELECT c.relname AS tab, con.conname AS con
                    FROM pg_constraint con
                    INNER JOIN pg_class c
                      ON con.conrelid = c.oid
                    WHERE con.contype='f';
            BEGIN
                FOR row IN statements LOOP
                    EXECUTE 'ALTER TABLE ' ||  quote_ident(row.tab) ||
                            ' ALTER CONSTRAINT ' || quote_ident(row.con) ||
                            ' DEFERRABLE;' ;
                END LOOP;
            END;
          $$;
      |] []

type TestCase = Postgres.ConnectionPool -> IO ()

mkConfig :: ConnectionPool
           -> IO Config
mkConfig pool = do
    runSqlPool cleanDB pool
    let conf =
          Config
          { configTimeout = Nothing
          , configOTPLength = 6
          , configOTPTimeoutSeconds = 10
          , configTFARequired = True
          , configOtp = Nothing
          , configUseTransactionLevels = False
          , configEmail = Just testEmailConfig
          , configAccountCreation = accountCreationConfig
          }
    return conf
  where
    -- | Delete all rows from all tables (Don't use TRUNACE TABLE since it's
    -- slower)
    cleanDB = P.rawExecute
      [sql|
         SET client_min_messages TO ERROR;
         SET CONSTRAINTS ALL DEFERRED;
         DO $$
         DECLARE
             statements CURSOR FOR
                 SELECT tablename FROM pg_tables
                 WHERE schemaname = 'public';
         BEGIN
             FOR stmt IN statements LOOP
                 EXECUTE 'DELETE FROM ' || quote_ident(stmt.tablename)
                   || ';';
             END LOOP;
         END;
         $$;
         RESET client_min_messages;
         |] []


withRunAPI :: (Config -> Config)
           -> ConnectionPool
           -> ((forall a. API a -> IO a) -> IO b)
           -> IO b
withRunAPI changeConf pool f = do
  conf <- mkConfig pool
  let apiState = ApiState { apiStateConfig = changeConf conf
                          , apiStateAuditSource = AuditSourceTest
                          }
  f $ runAPI pool apiState


seconds :: Integer -> NominalDiffTime
seconds s = fromIntegral s

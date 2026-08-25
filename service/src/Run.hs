{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Run where

import Api
import Audit
import Backend (addInstance, haveInstance)
import Config
import Control.Lens
import Control.Monad (unless)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text.Strict.Lens
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Persist.Postgresql
import Monad
import NejlaCommon (getDBConnectInfo, withDBPool)
import Network.HTTP.Types as HTTP
import Network.Socket qualified as Net
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Persist.Migration (doMigrate)
import Persist.Schema
import SignedAuth qualified
import System.Environment
import System.Exit
import System.IO
import Types
import User

logMiddleware :: Wai.Middleware
logMiddleware app req respond = app req respond'
 where
  debug f = hPutStrLn stderr $ "[Info#auth-service]" ++ f
  respond' res = do
    unless
      ( Wai.requestMethod req == "GET"
          && Wai.pathInfo req == ["status"]
          && isLocal (Wai.remoteHost req)
      )
      $ debug
      $ concat
        [ " "
        , fromBS (Wai.requestMethod req)
        , " "
        , fromBS (Wai.rawPathInfo req)
        , " > "
        , show . HTTP.statusCode $ Wai.responseStatus res
        , " "
        ]
    respond res
  fromBS = Text.unpack . Text.decodeUtf8With Text.lenientDecode
  isLocal Net.SockAddrUnix{} = True
  isLocal (Net.SockAddrInet _port haddr) =
    Net.hostAddressToTuple haddr == (127, 0, 0, 1)
  isLocal (Net.SockAddrInet6 _port _flow haddr6 _scope) =
    haddr6 == (0, 0, 0, 1)

runMain :: IO ()
runMain = do
  logLevel <-
    lookupEnv "LOG" >>= \case
      Nothing -> return LevelWarn
      Just r -> case map Char.toLower r of
        "debug" -> return LevelDebug
        "info" -> return LevelInfo
        "warn" -> return LevelWarn
        "error" -> return LevelError
        _ -> do
          hPutStrLn stderr $ "Unknown log level: " ++ r
          exitFailure
  runStderrLoggingT . filterLogger (\_source level -> level >= logLevel) $
    do
      confFile <- loadConf "auth_service"
      conf <- getAuthServiceConfig confFile
      let logM =
            if logLevel <= LevelInfo
              then logMiddleware
              else Prelude.id
      conInfo <- getDBConnectInfo confFile
      withDBPool conInfo 5 doMigrate $ \pool -> do
        args <- liftIO getArgs
        noncePool <- liftIO SignedAuth.newNoncePool
        -- AppState for CLI invocations
        let appState =
              ApiState
                { apiStateConfig = conf
                , apiStateAuditSource =
                    AuditSourceCli
                      { auditSourceCliArguments =
                          Text.pack <$> args
                      }
                , apiStateNoncePool = noncePool
                }
        let run :: forall a m. (MonadIO m) => API a -> m a
            run = liftIO . runAPI pool appState
        case args of
          ("adduser" : args') -> do
            res <- run $ addUser (args' ^.. each . packed)
            case res of
              Nothing -> liftIO $ do
                hPutStrLn stderr "Could not add user"
                exitFailure
              Just (UserID uid) -> liftIO $ print uid
          ("rmuser" : args') -> run $ removeUser (args' ^.. each . packed)
          ("chpass" : args') -> run $ changePassword args'
          ("addrole" : args') -> run $ addRole args'
          ("rmrole" : args') -> run $ removeRole args'
          ("newinstance" : args') -> run $ addInstance' args'
          ("addinstance" : args') -> run $ userAddInstance args'
          ("removeinstance" : args') -> run $ userRemoveInstance args'
          ("deactivateuser" : args') -> run $ userDeactivate' args'
          ("reactivateuser" : args') -> run $ userReactivate args'
          ["run"] -> do
            secrets <- getSecrets confFile
            run
              . Foldable.forM_
                ( accountCreationConfigDefaultInstances $
                    configAccountCreation conf
                )
              $ \inst -> do
                haveInstance inst >>= \case
                  Just{} ->
                    $logDebug $
                      "Instance "
                        <> (Text.pack $ show inst)
                        <> " already exists"
                  Nothing -> do
                    $logDebug $ "Adding default instance " <> (showInstanceID inst)
                    _ <- addInstance (Just inst) (showInstanceID inst)
                    return ()

            liftIO $ Warp.run 80 (logM $ serveAPI pool noncePool conf secrets)
          _ -> liftIO $ do
            hPutStrLn
              stderr
              "Usage: auth-service [run|adduser|rmuser|chpass|addrole|rmrole|newinstance|addinstance|removeinstance|\
              \deactivateuser|reactivateuser] [options]"
            exitFailure
 where
  showInstanceID (InstanceID iid) = UUID.toText iid

-- Compares the schema to the SQL server and prints out necessary changes. For
-- development.
checkMigration :: IO ()
checkMigration = runStderrLoggingT $ do
  withPostgresqlConn "host=localhost user=postgres" $ \(conn :: SqlBackend) -> do
    runReaderT (printMigration migrateAll) conn

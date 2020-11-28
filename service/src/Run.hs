-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.Encoding.Error    as Text
import           Data.Text.Strict.Lens
import           Database.Persist.Postgresql
import           Network.HTTP.Types          as HTTP
import qualified Network.Socket              as Net
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Handler.Warp    as Warp
import           System.Environment
import           System.Exit
import           System.IO

import           NejlaCommon                 (withPool, runPoolRetry)

import           Api
import           Audit
import           Config
import           Monad
import           Persist.Migration           (doMigrate)
import           Persist.Schema
import           Types
import           User

logMiddleware :: Wai.Middleware
logMiddleware app req respond = app req respond'
  where
    debug f = hPutStrLn stderr $ "[Info#auth-service]" ++ f
    respond' res = do
      unless ( Wai.requestMethod req == "GET"
             && Wai.pathInfo req == ["status"]
             && isLocal (Wai.remoteHost req)
             ) $ debug $ concat
        [ " "
        , fromBS (Wai.requestMethod req) , " "
        , fromBS (Wai.rawPathInfo req) , " > "
        , show . HTTP.statusCode $ Wai.responseStatus res
        , " "
        ]
      respond res
    fromBS = Text.unpack . Text.decodeUtf8With Text.lenientDecode
    isLocal Net.SockAddrUnix{} = True
    isLocal (Net.SockAddrInet _port haddr) =
      Net.hostAddressToTuple haddr == (127,0,0,1)
    isLocal (Net.SockAddrInet6 _port _flow haddr6 _scope) =
      haddr6 == (0, 0, 0, 1)
    isLocal _ = False

runMain :: IO ()
runMain = runStderrLoggingT . filterLogger (\_source level -> level >= LevelWarn)
       $ do
    confFile <- loadConf "auth_service"
    conf <- getAuthServiceConfig confFile
    mbLogging <- liftIO $ lookupEnv "log"
    let logM = case mbLogging of
                Just "true" -> logMiddleware
                _ -> Prelude.id

    withPool confFile 5 $ \pool -> do
        args <- liftIO getArgs
        let appState = ApiState { apiStateConfig = conf
                                , apiStateAuditSource =
                                  AuditSourceCli { auditSourceCliArguments =
                                                     Text.pack <$> args
                                                 }
                                }
        let run = liftIO . runAPI pool appState
        _ <- runPoolRetry pool doMigrate
        case args of
         ("adduser": args') -> do
             res <- run $ addUser (args' ^.. each . packed)
             case res of
              Nothing -> liftIO $ do
                  hPutStrLn stderr "Could not add user"
                  exitFailure
              Just (UserID uid) -> liftIO $ print uid
         ("chpass": args') -> run $ changePassword args'
         ("addrole" : args') -> run $ addRole args'
         ("rmrole" : args') -> run $ removeRole args'
         ("newinstance": args') -> run $ addInstance' args'
         ("addinstance": args') -> run $ userAddInstance args'
         ("removeinstance": args') -> run $ userRemoveInstance args'
         ("deactivateuser": args') -> run $ userDeactivate' args'
         ("reactivateuser": args') -> run $ userReactivate args'
         ["run"] -> liftIO $ Warp.run 80 (logM $ serveAPI pool conf)
         _ -> liftIO $ do
             hPutStrLn stderr
               "Usage: auth-service [run|adduser|chpass|addrole|rmrole|newinstance|addinstance|removeinstance|\
               \deactivateuser|eeactivateuser] [options]"
             exitFailure

-- Compares the schema to the SQL server and prints out necessary changes. For
-- development.
checkMigration :: IO ()
checkMigration = runStderrLoggingT $ do
  withPostgresqlConn "host=localhost user=postgres" $ \(conn :: SqlBackend) -> do
    runReaderT (printMigration migrateAll) conn

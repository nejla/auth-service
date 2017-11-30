-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Run where

import           Control.Lens
import           Control.Monad.Logger
import           Control.Monad.Trans
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import           Data.Text.Strict.Lens
import           Database.Persist.Postgresql
import           Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment
import           System.Exit
import           System.IO

import           NejlaCommon (withPool)

import           Api
import           Config
import           Persist.Schema
import           Types
import           User

logMiddleware :: Wai.Middleware
logMiddleware app req respond = do
  debug $ concat
    [ "< "
    , fromBS (Wai.requestMethod req) , " "
    , fromBS (Wai.rawPathInfo req)
    ]
  app req respond'
  where
    debug f = hPutStrLn stderr $ "[Info#auth-serivce]" ++ f
    respond' res = do
      debug $ concat
        ["> "
        , show . HTTP.statusCode $ Wai.responseStatus res
        , " "
        ]
      respond res
    fromBS = Text.unpack . Text.decodeUtf8With Text.lenientDecode

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
        let run = liftIO . runAPI pool conf
        _ <- liftIO $ runSqlPool (runMigrationSilent migrateAll) pool
        args <- liftIO getArgs
        case args of
         ("adduser": args') -> do
             res <- run $ addUser (args' ^.. each . packed)
             case res of
              Nothing -> liftIO $ do
                  hPutStrLn stderr "Could not add user"
                  exitFailure
              Just (UserID uid) -> liftIO $ print uid
         ("chpass": args') -> run $ changePassword args'
         ["run"] -> liftIO $ Warp.run 80 (logM $ serveAPI pool conf)
         _ -> liftIO $ do
             hPutStrLn stderr
               "Usage: auth-service [run|adduser|chpass] [options]"
             exitFailure

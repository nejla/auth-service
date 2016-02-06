-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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
    debug f = hPutStrLn stderr $ "[DEBUG#auth-serivce]" ++ f
    respond' res = do
      debug $ concat
        ["> "
        , show . HTTP.statusCode $ Wai.responseStatus res
        , " "
        ]
      respond res
    fromBS = Text.unpack . Text.decodeUtf8With Text.lenientDecode

main :: IO ()
main = runStderrLoggingT $ do
    confFile <- loadConf
    conf <- getAuthServiceConfig confFile
    let connectionString = conf ^. dbString
    liftIO . putStrLn $ "Connecting to DB " ++ show connectionString
    mbLogging <- liftIO $ lookupEnv "log"
    let logM = case mbLogging of
                Just "true" -> logMiddleware
                _ -> Prelude.id

    withPostgresqlPool connectionString 5 $ \pool -> do
        let run = liftIO . runAPI pool conf
        liftIO $ runSqlPool (runMigration migrateAll) pool
        args <- liftIO getArgs
        case args of
         ("adduser": args') -> do
             res <- run $ addUser (args' ^.. each . packed)
             case res of
              Nothing -> liftIO $ do
                  hPutStrLn stderr "Could not add user"
                  exitFailure
              Just () -> return ()
         ("chpass": args') -> run $ changePassword args'
         ["run"] -> liftIO $ Warp.run 80 (logM $ serveAPI pool conf)
         _ -> liftIO $ do
             hPutStrLn stderr
               "Usage: auth-service [run|adduser|chpass] [options]"
             exitFailure

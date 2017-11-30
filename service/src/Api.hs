-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Backend
import           Control.Monad.Except
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Database.Persist.Sql
import           Network.Wai
import           Servant
import           Types

import           Logging

import AuthService.Api

liftHandler :: IO a -> Handler a
liftHandler = Handler . lift

-- Will be transformed into X-Token header and token cookie by the nginx
serveLogin :: ConnectionPool -> Config -> Server LoginAPI
serveLogin pool conf loginReq = loginHandler
  where
    loginHandler = do
        mbReturnLogin <- liftHandler . runAPI pool conf $ login loginReq
        case mbReturnLogin of
         Right rl -> return (addHeader (returnLoginToken rl) rl)
         Left LoginErrorOTPRequired ->
             throwError ServantErr{ errHTTPCode = 499
                                  , errReasonPhrase = "OTP required"
                                  , errBody =
                                    "{\"error\":\"One time password required\"}"
                                  , errHeaders = []
                                  }
         Left _e -> throwError err403


serveLogout :: ConnectionPool -> Config -> Server LogoutAPI
serveLogout pool conf tok = logoutHandler
  where
    logoutHandler = do
        liftHandler . runAPI pool conf $ logOut tok

serverDisableSessions :: ConnectionPool -> Config -> Server DisableSessionsAPI
serverDisableSessions pool conf tok = disableSessionsHandler
  where
    disableSessionsHandler = do
      liftHandler . runAPI pool conf $ closeOtherSessions tok


serveChangePassword :: ConnectionPool -> Config -> Server ChangePasswordAPI
serveChangePassword pool conf tok chpass = chPassHandler
  where
    chPassHandler = do
      mbError <- liftHandler . runAPI pool conf $ changePassword tok chpass
      case mbError of
       Right _ -> return ()
       Left (ChangePasswordLoginError{}) -> throwError err403
       Left (ChangePasswordHashError{}) -> throwError err500
       Left (ChangePasswordTokenError{}) -> throwError err403

serveCheckToken :: ConnectionPool -> Config -> Server CheckTokenAPI
serveCheckToken pool conf tok inst req = checkTokenHandler
  where
    checkTokenHandler = do
        res <- liftHandler . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText tok
                       <> " for instance " <> showText inst
            checkTokenInstance (fromMaybe "" req) tok inst
        case res of
         Nothing -> throwError err403
         Just usr -> return $ (addHeader usr $ ReturnUser usr)

servePublicCheckToken :: ConnectionPool -> Config -> Server PublicCheckTokenAPI
servePublicCheckToken pool conf tok = checkTokenHandler
  where
    checkTokenHandler = do
        res <- liftHandler . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText tok
            checkToken tok
        case res of
         Nothing -> throwError err403
         Just _usr -> return ()


serveGetUserInstancesAPI :: ConnectionPool
                         -> Config
                         -> Server GetUserInstancesAPI
serveGetUserInstancesAPI pool conf usr =
  liftHandler . runAPI pool conf $ getUserInstances usr

serveGetUserInfoAPI :: ConnectionPool
                    -> Config
                    -> Server GetUserInfoAPI
serveGetUserInfoAPI pool conf tok =  do
  mbRUI <- liftHandler . runAPI pool conf $ getUserInfo tok
  case mbRUI of
   Nothing -> throwError err403
   Just rui -> return rui

--------------------------------------------------------------------------------
-- Admin interface -------------------------------------------------------------
--------------------------------------------------------------------------------

serveCreateUserAPI :: ConnectionPool -> Config -> Server CreateUserAPI
serveCreateUserAPI pool conf addUser = do
  res <- liftHandler . runAPI pool conf $ createUser addUser
  case res of
    Nothing -> throwError err500
    Just uid -> return $ ReturnUser uid

adminAPIPrx :: Proxy AdminAPI
adminAPIPrx = Proxy

serveAdminAPI :: ConnectionPool
              -> Config
              -> Server CreateUserAPI
serveAdminAPI pool conf = serveCreateUserAPI pool conf

--------------------------------------------------------------------------------
-- Interface -------------------------------------------------------------------
--------------------------------------------------------------------------------

apiPrx :: Proxy Api
apiPrx = Proxy

serveAPI :: ConnectionPool -> Config -> Application
serveAPI pool conf = serve apiPrx $ serveLogin pool conf
                               :<|> serveCheckToken pool conf
                               :<|> servePublicCheckToken pool conf
                               :<|> serveLogout pool conf
                               :<|> serverDisableSessions pool conf
                               :<|> serveChangePassword pool conf
                               :<|> serveGetUserInstancesAPI pool conf
                               :<|> serveGetUserInfoAPI pool conf
                               :<|> serveAdminAPI pool conf

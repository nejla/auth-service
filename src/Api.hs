-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api where

import           Backend
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Traversable
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Sql
import           Network.Wai
import           Servant
import           Types

import           Logging


type LoginAPI = "login"
              :> ReqBody '[JSON] Login
              :> Post '[JSON] (Headers '[Header "X-Token" B64Token] ReturnLogin)

serveLogin :: ConnectionPool -> Config -> Server LoginAPI
serveLogin pool conf loginReq = loginHandler
  where
    loginHandler = do
        mbReturnLogin <- lift . runAPI pool conf $ login loginReq
        case mbReturnLogin of
         Right rl -> return (addHeader (returnLoginToken rl) rl)
         Left LoginErrorOTPRequired ->
             left ServantErr{ errHTTPCode = 499
                            , errReasonPhrase = "OTP required"
                            , errBody =
                              "{\"error\":\"One time password required\"}"
                            , errHeaders = []
                            }
         Left _e -> left err403

type LogoutAPI = "logout"
               :> Capture "token" B64Token
               :> Post '[JSON] ()

serveLogout :: ConnectionPool -> Config -> Server LogoutAPI
serveLogout pool conf token = logoutHandler
  where
    logoutHandler = do
        lift . runAPI pool conf $ logOut token

type CheckTokenAPI = "check-token"
                  :> Capture "token" B64Token
                  :> Capture "instance" InstanceID
                  :> Get '[JSON] (Headers '[Header "X-User" UserID] ReturnUser)

serveCheckToken :: ConnectionPool -> Config -> Server CheckTokenAPI
serveCheckToken pool conf token inst = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText token
                       <> " for instance " <> showText inst
            mbUser <- checkToken token
            forM mbUser $ \user -> do
              checkInstance inst user
              return user
        case res of
         Nothing -> left err403
         Just usr -> return $ (addHeader usr $ ReturnUser usr)

type PublicCheckTokenAPI = "check-token"
                        :> Capture "token" B64Token
                        :> Get '[JSON] ()

servePublicCheckToken :: ConnectionPool -> Config -> Server PublicCheckTokenAPI
servePublicCheckToken pool conf token = checkTokenHandler
  where
    checkTokenHandler = do
        res <- lift . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText token
            checkToken token
        case res of
         Nothing -> left err403
         Just _usr -> return ()


type GetUserInstancesAPI = "user-instances"
                         :> Capture "user" UserID
                         :> Get '[JSON] [ReturnInstance]

serveGetUserInstancesAPI :: ConnectionPool
                         -> Config
                         -> Server GetUserInstancesAPI
serveGetUserInstancesAPI pool conf user =
  lift . runAPI pool conf $ getUserInstances user

type UserMirrorAPI = "showUser"
                   :> Header "X-User" Text
                   :> Get '[JSON] Text

serveUserMirror mbUser = do
    case mbUser of
     Nothing -> return "None"
     Just user -> return user

apiPrx :: Proxy (    LoginAPI
                :<|> CheckTokenAPI
                :<|> PublicCheckTokenAPI
                :<|> LogoutAPI
                :<|> GetUserInstancesAPI
                :<|> UserMirrorAPI)
apiPrx = Proxy

serveAPI :: ConnectionPool -> Config -> Application
serveAPI pool conf = serve apiPrx $ serveLogin pool conf
                               :<|> serveCheckToken pool conf
                               :<|> servePublicCheckToken pool conf
                               :<|> serveLogout pool conf
                               :<|> serveGetUserInstancesAPI pool conf
                               :<|> serveUserMirror

-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Backend
import           Control.Lens
import qualified Control.Monad.Catch  as Ex
import           Control.Monad.Except
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe, maybeToList)
import           Database.Persist.Sql
import           Network.Wai
import           Servant
import           Types

import           Logging              hiding (token)
import           PasswordReset
import qualified Persist.Schema       as DB

import           AuthService.Api

liftHandler :: IO a -> Handler a
liftHandler = Handler . lift

type StatusApi = "status" :> GetNoContent '[JSON] NoContent

serveStatus :: Server StatusApi
serveStatus = return NoContent


-- Will be transformed into X-Token header and token cookie by the nginx
serveLogin :: ConnectionPool -> Config -> Server LoginAPI
serveLogin pool conf loginReq = loginHandler
  where
    loginHandler = do
        mbReturnLogin <- liftHandler . runAPI pool conf $ login loginReq
        case mbReturnLogin of
         Right rl -> return (addHeader (returnLoginToken rl) rl)
         Left LoginErrorOTPRequired ->
             throwError ServerError{ errHTTPCode = 499
                                   , errReasonPhrase = "OTP required"
                                   , errBody =
                                     "{\"error\":\"One time password required\"}"
                                   , errHeaders = []
                                   }
         Left _e -> throwError err403


serveLogout :: ConnectionPool -> Config -> Server LogoutAPI
serveLogout pool conf tok = logoutHandler >> return NoContent
  where
    logoutHandler = liftHandler . runAPI pool conf $ logOut tok

serverDisableSessions :: ConnectionPool -> Config -> Server DisableSessionsAPI
serverDisableSessions pool conf tok = disableSessionsHandler >> return NoContent
  where
    disableSessionsHandler =
      liftHandler . runAPI pool conf $ closeOtherSessions tok


serveChangePassword :: ConnectionPool -> Config -> Server ChangePasswordAPI
serveChangePassword pool conf tok chpass = chPassHandler >> return NoContent
  where
    chPassHandler = do
      mbError <- liftHandler . runAPI pool conf $ changePassword tok chpass
      case mbError of
       Right _ -> return ()
       Left ChangePasswordLoginError{} -> throwError err403
       Left ChangePasswordHashError{} -> throwError err500
       Left ChangePasswordTokenError{} -> throwError err403
       Left ChangePasswordUserDoesNotExistError{} -> throwError err403

serveCheckToken :: ConnectionPool -> Config -> Server CheckTokenAPI
serveCheckToken pool conf tok inst req = checkTokenHandler
  where
    checkTokenHandler = do
      res <-
        liftHandler . runAPI pool conf $ do
          logDebug $
            "Checking token " <> showText tok <> " for instance " <>
            showText inst
          mbUser <- checkTokenInstance (fromMaybe "" req) tok inst
          forM mbUser $ \(usrId, usrEmail, userName) -> do
            roles' <- getUserRoles usrId
            return (usrId, usrEmail, userName, roles')
      case res of
        Nothing -> throwError err403
        Just (usr, userEmail, userName, roles') -> do
          return . addHeader usr
                 . addHeader userEmail
                 . addHeader userName
                 . addHeader (Roles roles')
                 $ ReturnUser { returnUserUser = usr
                              , returnUserRoles = roles'
                              }

servePublicCheckToken :: ConnectionPool -> Config -> Server PublicCheckTokenAPI
servePublicCheckToken pool conf tok = checkTokenHandler
  where
    checkTokenHandler = do
        res <- liftHandler . runAPI pool conf $ do
            logDebug $ "Checking token " <> showText tok
            checkToken tok
        case res of
         Nothing -> throwError err403
         Just _usr -> return NoContent


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
-- Admin interface
--------------------------------------------------------------------------------

serveCreateUserAPI :: ConnectionPool -> Config -> Server CreateUserAPI
serveCreateUserAPI pool conf addUser = do
  res <- liftHandler . runAPI pool conf $ createUser addUser
  case res of
    Nothing -> throwError err500
    Just uid -> return $ ReturnUser{ returnUserUser = uid
                                   , returnUserRoles = addUser ^. roles
                                   }

adminAPIPrx :: Proxy AdminAPI
adminAPIPrx = Proxy

serveAdminAPI :: ConnectionPool
              -> Config
              -> Server CreateUserAPI
serveAdminAPI = serveCreateUserAPI

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

apiPrx :: Proxy (StatusApi :<|> Api)
apiPrx = Proxy

serveRequestPasswordResetAPI ::
     ConnectionPool -> Config -> Server RequestPasswordResetAPI
serveRequestPasswordResetAPI pool conf req = do
  case (conf ^. email) of
    Nothing -> do
      liftHandler . runAPI pool conf $ logError $ "Password reset: email not configured"
      throwError $ err404
    Just _emailCfg -> do
      res <- Ex.try . liftHandler . runAPI pool conf $
               passwordResetRequest (req ^. email)
      case res of
        Left EmailErrorNotConfigured -> throwError err400
        Left EmailRenderError -> throwError err500
        Left EmailAddressUnknownError -> throwError err403
        Right False -> throwError err500
        Right True -> return NoContent

servePasswordResetAPI :: ConnectionPool -> Config -> Server PasswordResetAPI
servePasswordResetAPI pool conf pwReset = do
  mbError <-
    liftHandler . runAPI pool conf $
      resetPassword (pwReset ^. token) (pwReset ^. newPassword) (pwReset ^. Types.otp)
  case mbError of
    Left (ChangePasswordLoginError LoginErrorOTPRequired)
      -> throwError err403
    Left _ -> throwError err403
    Right () -> return NoContent

servePasswordResetTokenInfo ::
     ConnectionPool -> Config -> Server PasswordResetInfoAPI
servePasswordResetTokenInfo _pool _conf Nothing = throwError err400
servePasswordResetTokenInfo pool conf (Just token) = do
  mbInfo <-
    Ex.try . liftHandler . runAPI pool conf $ getUserByResetPwToken token
  case mbInfo of
    Right (Just r) -> return $ ResetTokenInfo (DB.userEmail r)
    Left ChangePasswordTokenError -> do
      liftHandler . runAPI pool conf $ logInfo $ "Tried to request token info with invalid Token " <> token
      throwError err403
    _ -> throwError err403

serveCreateAccountApi :: ConnectionPool -> Config -> Server CreateAccountAPI
serveCreateAccountApi pool conf xinstance createAccount =
  case (accountCreationConfigEnabled $ configAccountCreation conf) of
    False -> throwError err403
    True -> liftHandler . runAPI pool conf $ do
      dis <- getConfig (accountCreation . defaultInstances)
      _ <-
        createUser
          AddUser
          { addUserUuid = Nothing
          , addUserEmail = createAccountEmail createAccount
          , addUserPassword = createAccountPassword createAccount
          , addUserName = createAccountName createAccount
          , addUserPhone = createAccountPhone createAccount
          , addUserInstances = (List.nub $ maybeToList xinstance ++ dis)
          , addUserRoles = []
          }
      return NoContent

serveAPI :: ConnectionPool -> Config -> Application
serveAPI pool conf = serve apiPrx $ serveStatus
                               :<|> serveLogin pool conf
                               :<|> serveCheckToken pool conf
                               :<|> servePublicCheckToken pool conf
                               :<|> serveLogout pool conf
                               :<|> serverDisableSessions pool conf
                               :<|> serveChangePassword pool conf
                               :<|> serveGetUserInstancesAPI pool conf
                               :<|> serveGetUserInfoAPI pool conf
                               :<|> serveAdminAPI pool conf
                               :<|> serveRequestPasswordResetAPI pool conf
                               :<|> servePasswordResetAPI pool conf
                               :<|> servePasswordResetTokenInfo pool conf
                               :<|> serveCreateAccountApi pool conf

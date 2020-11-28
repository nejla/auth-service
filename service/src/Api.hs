{-# LANGUAGE LambdaCase #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# OPTIONS_GHC -fdefer-typed-holes #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Backend
import           Control.Lens         hiding (Context)
import qualified Control.Monad.Catch  as Ex
import           Control.Monad.Except
import           Data.Aeson           (encode)
import qualified Data.List            as List
import           Data.Maybe           (fromMaybe, maybeToList)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Database.Persist.Sql
import qualified NejlaCommon          as NC
import           Network.Wai
import           Servant
import           Types

import           Logging              hiding (token)
import           PasswordReset
import qualified Persist.Schema       as DB

import           Audit
import           AuthService.Api
import           Monad

--------------------------------------------------------------------------------
-- Api -------------------------------------------------------------------------
--------------------------------------------------------------------------------

showText :: Show a => a -> Text
showText = Text.pack . show

liftHandler :: IO a -> Handler a
liftHandler =
  Ex.handle (\(e :: NC.PersistError) -> throwError (toError e))
    . Handler . lift
  where
    toError e =
      ServerError{ errHTTPCode = NC.responseCode e
                 , errReasonPhrase = ""
                 , errBody = encode e
                 , errHeaders = []
                 }


type StatusApi = "status" :> GetNoContent '[JSON] NoContent

serveStatus :: Server StatusApi
serveStatus = return NoContent


-- Will be transformed into X-Token header and token cookie by the nginx
serveLogin :: ConnectionPool -> ApiState -> Server LoginAPI
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
                                   , errHeaders = [("Content-Type", "application/json")]
                                   }
         Left _e -> throwError err403{ errBody = "{\"error\": \"Credentials not accepted\"}"
                                     , errHeaders = [("Content-Type", "application/json")]
                                     }


serveLogout :: ConnectionPool -> ApiState -> Server LogoutAPI
serveLogout pool conf tok = logoutHandler >> return NoContent
  where
    logoutHandler = liftHandler . runAPI pool conf $ logOut tok

serverDisableSessions :: ConnectionPool -> ApiState -> Server DisableSessionsAPI
serverDisableSessions pool conf tok = disableSessionsHandler >> return NoContent
  where
    disableSessionsHandler =
      liftHandler . runAPI pool conf $ closeOtherSessions tok


serveChangePassword :: ConnectionPool -> ApiState -> Server ChangePasswordAPI
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

serveCheckToken :: ConnectionPool -> ApiState -> Server CheckTokenAPI
serveCheckToken pool conf req (Just tok) (Just inst) = checkTokenHandler
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
serveCheckToken _ _ _ _ _ = throwError err400

servePublicCheckToken :: ConnectionPool -> ApiState -> Server PublicCheckTokenAPI
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
                         -> ApiState
                         -> Server GetUserInstancesAPI
serveGetUserInstancesAPI pool conf usr =
  liftHandler . runAPI pool conf $ getUserInstances usr

serveGetUserInfoAPI :: ConnectionPool
                    -> ApiState
                    -> Server GetUserInfoAPI
serveGetUserInfoAPI pool conf tok =  do
  mbRUI <- liftHandler . runAPI pool conf $ getUserInfo tok
  case mbRUI of
   Nothing -> throwError err403
   Just rui -> return rui

--------------------------------------------------------------------------------
-- Admin interface
--------------------------------------------------------------------------------

isAdmin :: Text -> ConnectionPool -> ApiState -> Maybe B64Token -> Handler IsAdmin
isAdmin _ _ _ Nothing = throwError err403
isAdmin request pool conf (Just token) = do
  liftHandler (runAPI pool conf $ checkAdmin request token) >>= \case
    Nothing -> throwError err403
    Just isAdmin -> return isAdmin

serveCreateUserAPI :: ConnectionPool
                   -> ApiState
                   -> Maybe B64Token
                   -> Server CreateUserAPI
serveCreateUserAPI pool conf tok addUser = do
  _ <- isAdmin desc pool conf tok
  res <- liftHandler . runAPI pool conf $ do
    mbRes <- createUser addUser
    return mbRes
  case res of
    Nothing -> throwError err500
    Just uid -> do
      return $ ReturnUser{ returnUserUser = uid
                         , returnUserRoles = addUser ^. roles
                         }
  where
    desc = "create user " <> Text.pack (show addUser)

serveGetUsersAPI :: ConnectionPool
                 -> ApiState
                 -> Maybe B64Token
                 -> Server GetUsersAPI
serveGetUsersAPI pool conf tok = do
  _ <- isAdmin desc pool conf tok
  liftHandler $ runAPI pool conf $ getUsers
  where
    desc = "get users"


serveDeactivateUsersAPI :: ConnectionPool
                        -> ApiState
                        -> Maybe B64Token
                        -> Server DeactivateUserAPI
serveDeactivateUsersAPI pool conf tok uid body = do
  _ <- isAdmin desc pool conf tok
  liftHandler $ runAPI pool conf $  deactivateUser uid (body ^. deactivateAt)
  -- @TODO audit
  return NoContent
  where
    desc = "deactivateUser " <> Text.pack (show uid) <> " " <> Text.pack (show body)

serveReactivateUsersAPI :: ConnectionPool
                        -> ApiState
                        -> Maybe B64Token
                        -> Server ReactivateUserAPI
serveReactivateUsersAPI pool conf tok uid = do
    _ <- isAdmin desc pool conf tok
    liftHandler $ runAPI pool conf $  reactivateUser uid
    return NoContent
  where
    desc = "reactivateUser" <> Text.pack (show uid)

adminAPIPrx :: Proxy AdminAPI
adminAPIPrx = Proxy

serveAdminAPI :: ConnectionPool
              -> ApiState
              -> Server AdminAPI
serveAdminAPI pool conf tok =
       serveCreateUserAPI      pool conf tok
  :<|> serveGetUsersAPI        pool conf tok
  :<|> serveDeactivateUsersAPI pool conf tok
  :<|> serveReactivateUsersAPI pool conf tok

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

apiPrx :: Proxy (StatusApi :<|> Api)
apiPrx = Proxy

serveRequestPasswordResetAPI ::
     ConnectionPool -> ApiState -> Server RequestPasswordResetAPI
serveRequestPasswordResetAPI pool conf req = do
  case (conf ^. config . email) of
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

servePasswordResetAPI :: ConnectionPool -> ApiState -> Server PasswordResetAPI
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
     ConnectionPool -> ApiState -> Server PasswordResetInfoAPI
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

serveCreateAccountApi :: ConnectionPool -> ApiState -> Server CreateAccountAPI
serveCreateAccountApi pool conf xinstance createAccount =
  case (accountCreationConfigEnabled
          . configAccountCreation $ apiStateConfig  conf) of
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
serveAPI pool conf =
  Audit.withAuditHttp $ \auditHttp ->
    let ctx = ApiState { apiStateConfig = conf
                       , apiStateAuditSource = auditHttp
                       }
    in serve apiPrx $ serveStatus
                               :<|> serveLogin pool ctx
                               :<|> serveCheckToken pool ctx
                               :<|> servePublicCheckToken pool ctx
                               :<|> serveLogout pool ctx
                               :<|> serverDisableSessions pool ctx
                               :<|> serveChangePassword pool ctx
                               :<|> serveGetUserInstancesAPI pool ctx
                               :<|> serveGetUserInfoAPI pool ctx
                               :<|> serveAdminAPI pool ctx
                               :<|> serveRequestPasswordResetAPI pool ctx
                               :<|> servePasswordResetAPI pool ctx
                               :<|> servePasswordResetTokenInfo pool ctx
                               :<|> serveCreateAccountApi pool ctx

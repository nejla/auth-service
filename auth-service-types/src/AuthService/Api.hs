{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AuthService.Api where

import           AuthService.Types
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Servant.API

type LoginAPI = "login"
              :> ReqBody '[JSON] Login
              :> Post '[JSON] (Headers '[Header "X-Token" B64Token] ReturnLogin)

type LogoutAPI = "logout"
               :> Capture "token" B64Token
               :> Post '[JSON] NoContent

type DisableSessionsAPI = "disable-sessions"
                       :> Capture "token" B64Token
                       :> Post '[JSON] NoContent

type ChangePasswordAPI = "change-password"
                       :> Capture "token" B64Token
                       :> ReqBody '[JSON] ChangePassword
                       :> Post '[JSON] NoContent

type CheckTokenAPI = "check-token"
                  :> Capture "token" B64Token
                  :> Capture "instance" InstanceID
                  :> Header "X-Original-URI" Text
                  :> Get '[JSON] (Headers '[ Header "X-User-ID" UserID
                                           , Header "X-User-Email" Email
                                           , Header "X-User-Name" Name
                                           , Header "X-Roles" Roles
                                           ] ReturnUser)

type PublicCheckTokenAPI = "check-token"
                        :> Capture "token" B64Token
                        :> Get '[JSON] NoContent

type GetUserInstancesAPI = "user-instances"
                         :> Capture "user" UserID
                         :> Get '[JSON] [ReturnInstance]

type GetUserInfoAPI = "user-info-by-token"
                    :> Capture "token" B64Token
                    :> Get '[JSON] ReturnUserInfo

type RequestPasswordResetAPI =  "request-password-reset"
                             :> ReqBody '[JSON] PasswordResetRequest
                             :> Post '[JSON] NoContent

type PasswordResetAPI =  "reset-password"
                      :> ReqBody '[JSON] PasswordReset
                      :> Post '[JSON] NoContent

type PasswordResetInfoAPI =  "reset-password-info"
                             :> QueryParam "token" Text
                             :> Get '[JSON] ResetTokenInfo


type CreateAccountAPI = "create-account"
                      :> Header "X-Instance" InstanceID
                      :> ReqBody '[JSON] CreateAccount
                      :> PostCreated '[JSON] NoContent

--------------------------------------------------------------------------------
-- Admin Interface
--------------------------------------------------------------------------------

type CreateUserAPI = "users" :> ReqBody '[JSON] AddUser :> Post '[JSON] ReturnUser

type AdminAPI = "admin" :> CreateUserAPI

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

type Api = LoginAPI
           :<|> CheckTokenAPI
           :<|> PublicCheckTokenAPI
           :<|> LogoutAPI
           :<|> DisableSessionsAPI
           :<|> ChangePasswordAPI
           :<|> GetUserInstancesAPI
           :<|> GetUserInfoAPI
           :<|> AdminAPI
           :<|> RequestPasswordResetAPI
           :<|> PasswordResetAPI
           :<|> PasswordResetInfoAPI
           :<|> CreateAccountAPI

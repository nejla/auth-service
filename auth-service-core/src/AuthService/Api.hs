{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AuthService.Api where

import           AuthService.Types
import           Data.Text         (Text)
import           Servant.API       hiding (PostNoContent)

import           SignedAuth

import           Compat            (PostNoContent)

type LoginAPI = "login"
              :> ReqBody '[JSON] Login
              :> Post '[JSON] (Headers '[Header "X-Token" B64Token] ReturnLogin)

-- Should generate a redirect
type SSOLoginAPI = "sso" :> "login"
                 :> Header "X-Instance" InstanceID
                 :> Verb 'GET 303 '[ JSON ]
                 (Headers '[ Header "Location" Text
                           , Header "Cache-Control" Text
                           , Header "Pragma" Text
                           ]
                   SamlLoginRequest)

type SSOEnabledAPI = "sso" :> "enabled" :> Get '[ JSON ] SsoEnabled

type SSOAssertAPI = "sso" :> "assert"
            :> Header "X-Instance" InstanceID
            :> ReqBody '[FormUrlEncoded] SamlResponse
            :> Verb 'POST 303 '[JSON]
                (Headers '[ Header "X-Token" B64Token
                          , Header "Location" Text
                          ]
                 ReturnLogin
                )

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
                  :> Header "X-Original-URI" Text
                  :> Header "X-Token" B64Token
                  :> Header "X-Instance" InstanceID
                  :> Get '[JSON] (Headers '[ Header "X-Auth" (JWS AuthHeader)
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

type GetAllUsersAPI = "users" :> Get '[JSON] [ReturnUserInfo]

type GetUsersByRolesAPI = "users"
                       :> "by-role"
                       :> Capture "role" Text
                       :> Get '[JSON] [ReturnUserInfo]

type DeactivateUserAPI = "users"
                      :> Capture "user" UserID
                      :> "deactivate"
                      :> ReqBody '[JSON] DeactivateUser
                      :> PostNoContent

type DeleteUserAPI = "users"
                   :> Capture "user" UserID
                   :> DeleteNoContent

type ReactivateUserAPI = "users"
                    :> Capture "user" UserID
                    :> "reactivate"
                    :> PostNoContent

type AdminAPI = "admin"
                 :> Header "X-Token" B64Token
                 :> (CreateUserAPI
                      :<|> GetAllUsersAPI
                      :<|> GetUsersByRolesAPI
                      :<|> DeactivateUserAPI
                      :<|> ReactivateUserAPI
                      :<|> DeleteUserAPI
                    )

--------------------------------------------------------------------------------
-- Micro Service Interface -----------------------------------------------------
--------------------------------------------------------------------------------

type GetUsers = "users"
              :> "by-uid"
              :> QueryParams "uid" Text
              :> Get '[JSON] [FoundUserInfo]

type ServiceAPI = "service"
                :> Header "X-Token" Text
                :> GetUsers

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

type Api = SSOEnabledAPI
           :<|> LoginAPI
           :<|> SSOLoginAPI
           :<|> SSOAssertAPI
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
           :<|> ServiceAPI

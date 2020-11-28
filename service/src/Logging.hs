-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

-- | Import this module qualified like this:
--
-- >>> import qualified Logging as Log

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Logging
  ( module Logging
  ) where

import Control.Monad.Logger
import Control.Monad.Trans
import Data.Aeson.TH
import Data.Int
import Data.Text            (Text)
import NejlaCommon
import NejlaCommon.Helpers

import Types

logDebug, logInfo, logWarn, logError :: MonadLogger m => Text -> m ()
logDebug = logDebugNS "auth-service"
logInfo  = logDebugNS "auth-service"
logWarn  = logWarnNS  "auth-service"
logError = logErrorNS "auth-service"


logES :: (MonadIO m, MonadLogger m, IsLogEvent a) =>
         a
      -> m ()
logES = logEvent

type Request = Text
type OtpRef = Int64
type TokenRef = Int64

data AuthFailedReason = AuthFailedReasonWrongPassword
                      | AuthFailedReasonWrongOtp
                      deriving (Show)

data Event
  = OTPSent{ user :: !Email, otp:: !OtpRef}
  | AuthSuccess{ user:: !Email, tokenId :: !TokenRef}
  | AuthSuccessOTP{ user:: !Email, otp:: !OtpRef, tokenId :: !TokenRef}
  | AuthFailed{ user:: !Email, reason :: !AuthFailedReason}
  | Request{ user:: !Email, request :: !Request, tokenId :: !TokenRef}
  | RequestNoToken{request:: !Request, instanceId :: !InstanceID}
  | RequestInvalidToken{ request :: !Request
                       , token:: !Text
                       , instanceId :: !InstanceID
                       }
  | AdminRequestInvalidToken{ request :: !Text
                            , token:: !Text
                            }
  | AdminRequestNotAdmin{ request :: !Text
                        , token:: !Text
                        }
  | RequestInvalidInstance{ user :: !Email
                          , request :: !Request
                          , tokenId :: !TokenRef
                          , instanceId :: !InstanceID
                          }
  | Logout{ user ::  !Email
          , tokenId :: !TokenRef
          }
  | UserCreated {user :: !Email }
  | PasswordResetRequested {user :: !Email }
  | PasswordResetCompleted {user :: !Email, token :: !Text}
  | PasswordChangeFailed {user :: !Email }
  | PasswordChanged {user :: !Email }
  deriving (Show)

instance IsLogEvent Event where
  toLogEvent v@OTPSent{}                =
    eventDetails "otp_sent" v
  toLogEvent v@AuthSuccess{}            =
    eventDetails "auth_success" v
  toLogEvent v@AuthSuccessOTP{}         =
    eventDetails "auth_success" v
  toLogEvent v@AuthFailed{}             =
    eventDetails "auth_failed" v
  toLogEvent v@Request{}                =
    eventDetails "request" v
  toLogEvent v@RequestNoToken{}         =
    eventDetails "request_no_token" v
  toLogEvent v@RequestInvalidToken{}    =
    eventDetails "request_invalid_token" v
  toLogEvent v@AdminRequestInvalidToken{}    =
    eventDetails "admin_request_invalid_token" v
  toLogEvent v@AdminRequestNotAdmin{}    =
    eventDetails "admin_request_not_admin" v
  toLogEvent v@RequestInvalidInstance{} =
    eventDetails "request_invalid_instance" v
  toLogEvent v@Logout{}                 =
    eventDetails "logout" v
  toLogEvent v@UserCreated{}            =
    eventDetails "user_created" v
  toLogEvent v@PasswordResetRequested{} =
    eventDetails "password_reset_requested" v
  toLogEvent v@PasswordResetCompleted{} =
    eventDetails "password_reset_completed" v
  toLogEvent v@PasswordChangeFailed{}   =
    eventDetails "password_change_failed" v
  toLogEvent v@PasswordChanged{}        =
    eventDetails "password_changed" v

deriveJSON (defaultOptions{constructorTagModifier =
                              cctu "_" . downcase . withoutPrefix "AuthFailedReason"
                          }
           ) ''AuthFailedReason


deriveJSON defaultOptions{ sumEncoding = TaggedObject "type" "contents"
                         , constructorTagModifier =
                              cctu "_"
                         }
           ''Event

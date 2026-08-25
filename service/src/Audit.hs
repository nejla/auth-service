{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Audit logging
--
-- This log should contain a trail of _changes to the state_, e.g. we log when a
-- user's password is changed. It does not includes events that do not lead to
-- changes to the database, e.g. failed queries or checking an existing token
module Audit where

import AuthService.Types
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Database.Persist qualified as P
import Database.Persist.Sql qualified as P
import NejlaCommon.Helpers
import Network.Socket
import Network.Wai qualified as Wai
import Numeric (showHex)
import Persist.Schema qualified as DB

data AuditSource
  = AuditSourceCli {auditSourceCliArguments :: [Text]}
  | AuditSourceHTTP
      { auditSourceHttpMethod :: Text
      , auditSourceHttpSourceIP :: Text
      , auditSourceHttpURL :: Text
      , auditSourceHttpHeaders :: [(Text, Text)]
      }
  | AuditSourceManual
  | AuditSourceTest

Aeson.deriveJSON
  ( (aesonTHOptions "auditSource")
      { Aeson.sumEncoding =
          Aeson.TaggedObject
            { Aeson.tagFieldName = "source"
            , Aeson.contentsFieldName = "details"
            }
      }
  )
  ''AuditSource

auditSourceType :: AuditSource -> Text
auditSourceType AuditSourceCli{} = "cli"
auditSourceType AuditSourceHTTP{} = "http"
auditSourceType AuditSourceManual = "manual"
auditSourceType AuditSourceTest = "test"

data AuditEvent
  = AuditUserCreated
      { auditUserID :: UserID
      , auditUserInstances :: [InstanceID]
      , auditUserRoles :: [Text]
      }
  | AuditUserRemoved
      { auditUserID :: UserID
      }
  | AuditUserRoleAdded
      { auditUserID :: UserID
      , auditUserRole :: Text
      }
  | AuditUserRoleRemoved
      { auditUserID :: UserID
      , auditUserRole :: Text
      }
  | AuditResetTokenCreated
      { auditUserID :: UserID
      , auditToken :: Text
      , auditExpires :: UTCTime
      }
  | AuditUserPasswordChanged
      { auditUserID :: UserID
      , auditResetToken :: Maybe Text
      , auditNewPasswordHash :: Text
      }
  | AuditInstanceAdded
      { auditInstanceID :: InstanceID
      , auditInstanceName :: Text
      }
  | AuditUserInstanceAdded
      { auditUserID :: UserID
      , auditInstanceID :: InstanceID
      }
  | AuditUserInstanceRemoved
      { auditUserID :: UserID
      , auditInstanceID :: InstanceID
      }
  | AuditOTPCreated
      { auditUserID :: UserID
      , auditOTP :: Text
      , auditPhone :: Text
      }
  | AuditTokenCreated
      { auditUserID :: UserID
      , auditOTPUsed :: Maybe Text
      , auditCreatedToken :: Text
      }
  | AuditSsoTokenCreated
      { auditSsoUserID :: Text
      , auditCreatedToken :: Text
      }
  | AuditTokenDeactivated
      {auditToken :: Text}
  | AuditOtherTokensDeactivated
      {auditToken :: Text}
  | AuditUserDeactivated
      { auditUserID :: UserID
      , auditDeactivateAt :: UTCTime
      }
  | AuditUserReactivated
      {auditUserID :: UserID}
  | AuditUserDeleted
      {auditUserID :: UserID}
  deriving (Show)

Aeson.deriveJSON
  ( (aesonTHOptions "audit")
      { Aeson.sumEncoding =
          Aeson.TaggedObject
            { Aeson.tagFieldName = "event"
            , Aeson.contentsFieldName = "details"
            }
      }
  )
  ''AuditEvent

auditLogType :: AuditEvent -> Text
auditLogType AuditUserCreated{} = "user created"
auditLogType AuditUserRemoved{} = "user removed"
auditLogType AuditUserRoleAdded{} = "role added"
auditLogType AuditUserRoleRemoved{} = "role removed"
auditLogType AuditResetTokenCreated{} = "password reset token created"
auditLogType AuditUserPasswordChanged{} = "user password changed"
auditLogType AuditInstanceAdded{} = "instance added"
auditLogType AuditUserInstanceAdded{} = "user instance added"
auditLogType AuditUserInstanceRemoved{} = "user instance removed"
auditLogType AuditOTPCreated{} = "OTP created"
auditLogType AuditTokenCreated{} = "login token created"
auditLogType AuditSsoTokenCreated{} = "login token created"
auditLogType AuditTokenDeactivated{} = "login token deactivated"
auditLogType AuditOtherTokensDeactivated{} = "other login tokens deactivated"
auditLogType AuditUserDeactivated{} = "user deactivated"
auditLogType AuditUserReactivated{} = "user reactivated"
auditLogType AuditUserDeleted{} = "user information redacted"

-- | Retrieve the user ID from the event
auditLogUser :: AuditEvent -> Maybe UserID
auditLogUser AuditUserCreated{auditUserID = uid} = Just uid
auditLogUser AuditUserRemoved{auditUserID = uid} = Just uid
auditLogUser AuditUserRoleAdded{auditUserID = uid} = Just uid
auditLogUser AuditUserRoleRemoved{auditUserID = uid} = Just uid
auditLogUser AuditResetTokenCreated{auditUserID = uid} = Just uid
auditLogUser AuditUserPasswordChanged{auditUserID = uid} = Just uid
auditLogUser AuditInstanceAdded{} = Nothing
auditLogUser AuditUserInstanceAdded{auditUserID = uid} = Just uid
auditLogUser AuditUserInstanceRemoved{auditUserID = uid} = Just uid
auditLogUser AuditOTPCreated{auditUserID = uid} = Just uid
auditLogUser AuditTokenCreated{auditUserID = uid} = Just uid
auditLogUser AuditSsoTokenCreated{} = Nothing
auditLogUser AuditTokenDeactivated{} = Nothing
auditLogUser AuditOtherTokensDeactivated{} = Nothing
auditLogUser AuditUserDeactivated{auditUserID = uid} = Just uid
auditLogUser AuditUserReactivated{auditUserID = uid} = Just uid
auditLogUser AuditUserDeleted{auditUserID = uid} = Just uid

addAuditLog :: AuditSource -> AuditEvent -> ReaderT P.SqlBackend IO ()
addAuditLog source event = do
  now <- liftIO getCurrentTime
  _ <-
    P.insert
      DB.AuditLog
        { DB.auditLogTime = now
        , DB.auditLogEvent = auditLogType event
        , DB.auditLogDetails = Text.decodeUtf8 . BSL.toStrict $ Aeson.encode event
        , DB.auditLogSource = auditSourceType source
        , DB.auditLogSourceDetails =
            Text.decodeUtf8With Text.lenientDecode
              . BSL.toStrict
              $ Aeson.encode source
        , DB.auditLogUser = auditLogUser event
        }
  return ()

-- -- | Extract the request information needed in the audit log
withAuditHttp :: (AuditSource -> Wai.Application) -> Wai.Application
withAuditHttp app req next =
  let ah =
        AuditSourceHTTP
          { auditSourceHttpMethod = decode $ Wai.requestMethod req
          , auditSourceHttpSourceIP = sourceIP
          , auditSourceHttpURL = decode $ Wai.rawPathInfo req
          , auditSourceHttpHeaders = fromHeader <$> Wai.requestHeaders req
          }
   in app ah req next
 where
  -- We expect to sit behind a proxy so the request IP isn't the IP we're
  -- interested in.
  --
  -- TODO: Ensure that clients can't spoof the "X-Forwarded-For"
  sourceIP =
    if
      | Just ip <- List.lookup "X-Forwarded-For" (Wai.requestHeaders req) ->
          decode ip
      | Just ip <- List.lookup "X-Real-IP" (Wai.requestHeaders req) ->
          decode ip
      | otherwise ->
          Text.pack $ showAddress $ Wai.remoteHost req
  decode = Text.decodeUtf8With Text.lenientDecode
  fromHeader (name, val) = (decode $ CI.original name, decode val)

showAddress :: SockAddr -> String
showAddress (SockAddrUnix str) = "unix: " <> str
showAddress (SockAddrInet _ addr) =
  let (i1, i2, i3, i4) = hostAddressToTuple addr
   in List.intercalate "." $ show <$> [i1, i2, i3, i4]
showAddress (SockAddrInet6 _ _ addr _)
  | end - begin > 1 =
      showFields prefix . showString "::" . showFields suffix $ ""
  | otherwise = showFields fields ""
 where
  showFields = foldr (.) Prelude.id . List.intersperse (showChar ':') . map showHex
  fields =
    let (i1, i2, i3, i4, i5, i6, i7, i8) = hostAddress6ToTuple addr
     in [i1, i2, i3, i4, i5, i6, i7, i8]
  prefix = take begin fields
  suffix = drop end fields
  begin = end + diff
  (diff, end) =
    minimum $
      scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0 ..]

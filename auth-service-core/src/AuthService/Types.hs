-- All rights reserved

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module AuthService.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString            (ByteString)
import           Data.ByteString.Conversion
import           Data.Data
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime)
import qualified Data.UUID                  as UUID
import qualified Web.FormUrlEncoded         as Form
import           Web.HttpApiData
import           Web.PathPieces

import           Helpers

newtype InstanceID = InstanceID { unInstanceID :: UUID.UUID }
                 deriving ( Show, Read, Eq, Ord, Typeable, Data
                          )

makePrisms ''InstanceID

instance PathPiece InstanceID where
    fromPathPiece = fmap InstanceID . UUID.fromText
    toPathPiece = Text.pack . UUID.toString . unInstanceID

instance ToHttpApiData InstanceID where
    toUrlPiece = toPathPiece

instance FromHttpApiData InstanceID where
    parseUrlPiece txt =
        case fromPathPiece txt of
         Nothing -> Left $ "Could not parse user id " <> txt
         Just uuid -> Right uuid

newtype UserID = UserID { unUserID :: UUID.UUID }
                 deriving ( Show, Read, Eq, Ord, Typeable, Data
                          )

makePrisms ''UserID

instance ToJSON InstanceID where
    toJSON (InstanceID uid) = toJSON $ UUID.toText uid

instance FromJSON InstanceID where
    parseJSON v = do
        txt <- parseJSON v
        case UUID.fromText txt of
         Nothing -> fail $ "Can't parse UUID " <> Text.unpack txt
         Just uuid -> return $ InstanceID uuid

instance ToByteString InstanceID where
    builder = builder . Text.encodeUtf8 . UUID.toText . unInstanceID

instance PathPiece UserID where
    fromPathPiece = fmap UserID . UUID.fromText
    toPathPiece = Text.pack . UUID.toString . unUserID

instance ToHttpApiData UserID where
    toUrlPiece = toPathPiece

instance FromHttpApiData UserID where
    parseUrlPiece txt =
        case fromPathPiece txt of
         Nothing -> Left $ "Could not parse user id " <> txt
         Just uuid -> Right uuid

instance ToJSON UserID where
    toJSON (UserID uid) = toJSON $ UUID.toText uid

instance FromJSON UserID where
    parseJSON v = do
        txt <- parseJSON v
        case UUID.fromText txt of
         Nothing -> fail $ "Can't parse UUID " <> Text.unpack txt
         Just uuid -> return $ UserID uuid

instance ToByteString UserID where
    builder = builder . Text.encodeUtf8 . UUID.toText . unUserID

newtype Name = Name{ unName :: Text}
             deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                      , ToJSON, FromJSON
                      , IsString, ToByteString
                      , ToHttpApiData, FromHttpApiData
                      )

makePrisms ''Name

newtype Password = Password{ unPassword :: Text}
                   deriving ( Show, Read, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Password

newtype PasswordHash = PasswordHash{ unPasswordHash :: ByteString}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            )

makePrisms ''PasswordHash

newtype Email = Email{ unEmail :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Email

newtype Phone = Phone { unPhone :: Text}
                   deriving ( Show, Eq, Ord, Typeable, Data
                            , ToJSON, FromJSON
                            , IsString
                            , ToHttpApiData, FromHttpApiData
                            )

makePrisms ''Phone

newtype B64Token = B64Token { unB64Token :: Text }
                   deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece
                            , ToByteString
                            , ToHttpApiData, FromHttpApiData
                            , ToJSON, FromJSON
                            )

makePrisms ''B64Token

type Role = Text

data AuthHeader = AuthHeader { authHeaderUserID :: Text
                             , authHeaderEmail :: Email
                             , authHeaderName :: Name
                             , authHeaderRoles :: [Role]
                             }

makeLensesWith camelCaseFields ''AuthHeader
deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "authHeader"} ''AuthHeader

--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------


data AddUser = AddUser { addUserUuid      :: Maybe UserID
                       , addUserEmail     :: Email
                       , addUserPassword  :: Password
                       , addUserName      :: Name
                       , addUserPhone     :: Maybe Phone
                       , addUserInstances :: [InstanceID]
                       , addUserRoles     :: [Text]
                       } deriving (Show)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "addUser"} ''AddUser
makeLensesWith camelCaseFields ''AddUser


data ReturnUser = ReturnUser { returnUserUser :: Text
                             , returnUserRoles :: [Role]
                             }
                    deriving (Show, Eq)

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnUser"}
    ''ReturnUser
makeLensesWith camelCaseFields ''ReturnUser

data ReturnInstance = ReturnInstance { returnInstanceName :: Text
                                     , returnInstanceId :: InstanceID
                                     } deriving ( Show, Read, Eq, Ord
                                                , Typeable, Data )

makeLensesWith camelCaseFields ''ReturnInstance

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnInstance"}
    ''ReturnInstance

data ReturnUserInfo = ReturnUserInfo { returnUserInfoId :: Text
                                     , returnUserInfoEmail :: Email
                                     , returnUserInfoName :: Name
                                     , returnUserInfoPhone :: Maybe Phone
                                     , returnUserInfoInstances :: [ReturnInstance]
                                     , returnUserInfoRoles :: [Text]
                                     , returnUserInfoDeactivate :: Maybe UTCTime
                                     } deriving Show

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnUserInfo"}
           ''ReturnUserInfo
makeLensesWith camelCaseFields ''ReturnUserInfo

-- | User info if found
data FoundUserInfo = FoundUserInfo
  { foundUserInfoId :: Text
  , foundUserInfoInfo :: Maybe ReturnUserInfo
  } deriving Show

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "foundUserInfo"}
           ''FoundUserInfo
makeLensesWith camelCaseFields ''FoundUserInfo


data Login = Login { loginUser     :: Email
                   , loginPassword :: Password
                   , loginOtp      :: Maybe Password
                   } deriving ( Show, Eq, Ord, Typeable, Data )

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "login"} ''Login
makeLensesWith camelCaseFields ''Login

data ReturnLogin = ReturnLogin { returnLoginToken :: B64Token
                               , returnLoginInstances :: [ReturnInstance]
                               } deriving ( Show, Read, Eq, Ord
                                          , Typeable, Data )

makeLensesWith camelCaseFields ''ReturnLogin

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "returnLogin"}
    ''ReturnLogin

data ChangePassword = ChangePassword { changePasswordOldPasword :: Password
                                     , changePasswordNewPassword :: Password
                                     , changePasswordOtp :: Maybe Password
                                     } deriving ( Show, Eq, Ord, Typeable, Data )

deriveJSON defaultOptions{fieldLabelModifier = dropPrefix "changePassword"}
           ''ChangePassword
makeLensesWith camelCaseFields ''ChangePassword

--------------------------------------------------------------------------------
-- Password Reset
--------------------------------------------------------------------------------

newtype PasswordResetRequest =
  PasswordResetRequest
  { passwordResetRequestEmail :: Email
  } deriving (Show)

makeLensesWith camelCaseFields ''PasswordResetRequest

deriveJSON
  defaultOptions {fieldLabelModifier = dropPrefix "passwordResetRequest"}
  ''PasswordResetRequest

newtype ResetTokenInfo =
  ResetTokenInfo
  { resetTokenInfoEmail :: Email
  } deriving (Show)

makeLensesWith camelCaseFields ''ResetTokenInfo

deriveJSON
  defaultOptions {fieldLabelModifier = dropPrefix "resetTokenInfo"}
  ''ResetTokenInfo

data PasswordReset =
  PasswordReset
  { passwordResetToken :: Text
  , passwordResetOtp   :: Maybe Password
  , passwordResetNewPassword :: Password
  } deriving (Show)


makeLensesWith camelCaseFields ''PasswordReset

deriveJSON
  defaultOptions {fieldLabelModifier = dropPrefix "passwordReset"}
  ''PasswordReset

--------------------------------------------------------------------------------
-- Account Creation
--------------------------------------------------------------------------------

data CreateAccount =
  CreateAccount
  { createAccountEmail :: Email
  , createAccountPassword :: Password
  , createAccountName :: Name
  , createAccountPhone :: Maybe Phone
  } deriving (Show)

makeLensesWith camelCaseFields ''CreateAccount

deriveJSON
  defaultOptions {fieldLabelModifier = dropPrefix "createAccount"}
  ''CreateAccount

--------------------------------------------------------------------------------
-- Account Disabling -----------------------------------------------------------
--------------------------------------------------------------------------------

data DeactivateAt = DeactivateNow
                  | DeactivateAt UTCTime
                  deriving (Show)

instance ToJSON DeactivateAt where
  toJSON DeactivateNow = String "now"
  toJSON (DeactivateAt time) = toJSON time

instance FromJSON DeactivateAt where
  parseJSON (String "now") = return DeactivateNow
  parseJSON v = DeactivateAt <$> parseJSON v

makePrisms ''DeactivateAt

newtype DeactivateUser =
  DeactivateUser
  { deactivateUserDeactivateAt :: DeactivateAt
  } deriving (Show)

makeLensesWith camelCaseFields ''DeactivateUser

deriveJSON
  defaultOptions {fieldLabelModifier = camelTo2 '_' . dropPrefix "deactivateUser"}
  ''DeactivateUser

--------------------------------------------------------------------------------
-- SAML sso --------------------------------------------------------------------
--------------------------------------------------------------------------------

data SamlResponse =
  SamlResponse
  { samlResponseBody :: Text
  , samlResponseRelayState :: Maybe Text
  } deriving (Show)

instance Form.FromForm SamlResponse where
  fromForm f =
    SamlResponse
    <$> Form.parseUnique "SAMLResponse" f
    <*> Form.parseMaybe "RelayState" f

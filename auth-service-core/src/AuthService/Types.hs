-- All rights reserved
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module AuthService.Types where

import           AuthService.OpenAPI.Schema

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString            ( ByteString )
import           Data.ByteString.Conversion
import           Data.Data
import qualified Data.OpenApi               as OpenApi
import           Data.String
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            ( UTCTime )
import qualified Data.UUID                  as UUID

import           GHC.Generics               ( Generic )

import           Helpers

import qualified Web.FormUrlEncoded         as Form
import           Web.HttpApiData
import           Web.PathPieces

newtype InstanceID =
  InstanceID
  { unInstanceID :: UUID.UUID
  }
    deriving ( Show, Read, Eq, Ord, Typeable, Data
             , OpenApi.ToParamSchema , OpenApi.ToSchema)

makePrisms ''InstanceID

instance PathPiece InstanceID where
  fromPathPiece = fmap InstanceID . UUID.fromText

  toPathPiece = Text.pack . UUID.toString . unInstanceID

instance ToHttpApiData InstanceID where
  toUrlPiece = toPathPiece

instance FromHttpApiData InstanceID where
  parseUrlPiece txt = case fromPathPiece txt of
      Nothing -> Left $ "Could not parse user id " <> txt
      Just uuid -> Right uuid

newtype UserID =
  UserID
  { unUserID :: UUID.UUID
  }
    deriving ( Show, Read, Eq, Ord, Typeable
             , OpenApi.ToParamSchema, OpenApi.ToSchema)

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
  parseUrlPiece txt = case fromPathPiece txt of
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

newtype Name =
  Name
  { unName :: Text
  }
    deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece, ToJSON, FromJSON
             , IsString, ToByteString, ToHttpApiData, FromHttpApiData
             , OpenApi.ToParamSchema, OpenApi.ToSchema )

makePrisms ''Name

newtype Password =
  Password
  { unPassword :: Text
  }
    deriving ( Show, Read, Eq, Ord, Typeable, Data, ToJSON, FromJSON, IsString
             , ToHttpApiData, FromHttpApiData, OpenApi.ToParamSchema
             , OpenApi.ToSchema
             )

makePrisms ''Password

newtype PasswordHash =
  PasswordHash
  { unPasswordHash :: ByteString
  }
    deriving ( Show, Eq, Ord, Typeable, Data)

makePrisms ''PasswordHash

newtype Email =
  Email
  { unEmail :: Text
  }
    deriving ( Show, Eq, Ord, Typeable, Data, ToJSON, FromJSON, IsString
             , ToHttpApiData, FromHttpApiData, OpenApi.ToSchema)

makePrisms ''Email

newtype Phone =
  Phone
  { unPhone :: Text
  }
    deriving ( Show, Eq, Ord, Typeable, Data, ToJSON, FromJSON, IsString
             , ToHttpApiData, FromHttpApiData, OpenApi.ToSchema )

makePrisms ''Phone

newtype B64Token =
  B64Token
  { unB64Token :: Text
  }
    deriving ( Show, Read, Eq, Ord, Typeable, Data, PathPiece, ToByteString
             , ToHttpApiData, FromHttpApiData, ToJSON, FromJSON, OpenApi.ToSchema )

makePrisms ''B64Token

instance OpenApi.ToParamSchema B64Token where
  toParamSchema _ = OpenApi.toParamSchema (Proxy :: Proxy String)

type Role = Text

data AuthHeader =
  AuthHeader
  { authHeaderUserID :: Text
  , authHeaderEmail  :: Email
  , authHeaderName   :: Name
  , authHeaderRoles  :: [Role]
  }
    deriving ( Generic )
    deriving ( ToJSON, FromJSON, OpenApi.ToSchema ) via (JSONStruct AuthHeader)

makeLensesWith camelCaseFields ''AuthHeader

--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------
data AddUser =
  AddUser
  { addUserUuid      :: Maybe UserID
  , addUserEmail     :: Email
  , addUserPassword  :: Password
  , addUserName      :: Name
  , addUserPhone     :: Maybe Phone
  , addUserInstances :: [InstanceID]
  , addUserRoles     :: [Text]
  }
    deriving ( Show, Generic )
    deriving ( ToJSON, FromJSON, OpenApi.ToSchema ) via (JSONStruct AddUser)

makeLensesWith camelCaseFields ''AddUser

data ReturnUser =
  ReturnUser
  { returnUserUser  :: Text
  , returnUserRoles :: [Role]
  }
    deriving ( Show, Eq, Generic )
    deriving ( ToJSON, FromJSON, OpenApi.ToSchema ) via (JSONStruct ReturnUser)

makeLensesWith camelCaseFields ''ReturnUser

data ReturnInstance =
  ReturnInstance
  { returnInstanceName :: Text
  , returnInstanceId   :: InstanceID
  }
    deriving (Generic,  Show, Read, Eq, Ord, Typeable, Data )
    deriving ( ToJSON, FromJSON
             , OpenApi.ToSchema ) via (JSONStruct ReturnInstance)

makeLensesWith camelCaseFields ''ReturnInstance

data ReturnUserInfo =
  ReturnUserInfo
  { returnUserInfoId         :: Text
  , returnUserInfoEmail      :: Email
  , returnUserInfoName       :: Name
  , returnUserInfoPhone      :: Maybe Phone
  , returnUserInfoInstances  :: [ReturnInstance]
  , returnUserInfoRoles      :: [Text]
  , returnUserInfoDeactivate :: Maybe UTCTime
  }
    deriving (Generic,  Show )
    deriving ( ToJSON, FromJSON
             , OpenApi.ToSchema ) via (JSONStruct ReturnUserInfo)

makeLensesWith camelCaseFields ''ReturnUserInfo

-- | User info if found
data FoundUserInfo =
  FoundUserInfo
  { foundUserInfoId   :: Text
  , foundUserInfoInfo :: Maybe ReturnUserInfo
  }
    deriving (Generic,  Show )
    deriving ( ToJSON, FromJSON
             , OpenApi.ToSchema ) via (JSONStruct FoundUserInfo)

makeLensesWith camelCaseFields ''FoundUserInfo

data Login =
  Login
  { loginUser     :: Email
  , loginPassword :: Password
  , loginOtp      :: Maybe Password
  }
    deriving (Generic,  Show, Eq, Ord, Typeable, Data )
    deriving ( ToJSON, FromJSON, OpenApi.ToSchema ) via (JSONStruct Login)

makeLensesWith camelCaseFields ''Login

data ReturnLogin =
  ReturnLogin
  { returnLoginToken     :: B64Token
  , returnLoginInstances :: [ReturnInstance]
  }
    deriving (Generic,  Show, Read, Eq, Ord, Typeable, Data )
    deriving ( ToJSON, FromJSON , OpenApi.ToSchema ) via (JSONStruct ReturnLogin)

makeLensesWith camelCaseFields ''ReturnLogin

data ChangePassword =
  ChangePassword
  { changePasswordOldPassword :: Password
  , changePasswordNewPassword :: Password
  , changePasswordOtp         :: Maybe Password
  }
    deriving (Generic,  Show, Eq, Ord, Typeable, Data )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema) via (JSONStruct ChangePassword)

makeLensesWith camelCaseFields ''ChangePassword

--------------------------------------------------------------------------------
-- Password Reset
--------------------------------------------------------------------------------
newtype PasswordResetRequest =
  PasswordResetRequest
  { passwordResetRequestEmail :: Email
  }
    deriving (Generic,  Show )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct PasswordResetRequest)

makeLensesWith camelCaseFields ''PasswordResetRequest


newtype ResetTokenInfo =
  ResetTokenInfo
  { resetTokenInfoEmail :: Email
  }
    deriving (Generic,  Show )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct ResetTokenInfo)


makeLensesWith camelCaseFields ''ResetTokenInfo


data PasswordReset =
  PasswordReset
  { passwordResetToken       :: Text
  , passwordResetOtp         :: Maybe Password
  , passwordResetNewPassword :: Password
  }
    deriving (Generic,  Show )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct PasswordReset)


makeLensesWith camelCaseFields ''PasswordReset


--------------------------------------------------------------------------------
-- Account Creation
--------------------------------------------------------------------------------
data CreateAccount =
  CreateAccount
  { createAccountEmail    :: Email
  , createAccountPassword :: Password
  , createAccountName     :: Name
  , createAccountPhone    :: Maybe Phone
  }
    deriving (Generic,  Show )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct CreateAccount)


makeLensesWith camelCaseFields ''CreateAccount


--------------------------------------------------------------------------------
-- Account Disabling -----------------------------------------------------------
--------------------------------------------------------------------------------
data DeactivateAt
  = DeactivateNow
  | DeactivateAt UTCTime
    deriving (Generic,  Show )

instance ToJSON DeactivateAt where
  toJSON DeactivateNow = String "now"
  toJSON (DeactivateAt time) = toJSON time

instance FromJSON DeactivateAt where
  parseJSON (String "now") = return DeactivateNow
  parseJSON v = DeactivateAt <$> parseJSON v

instance OpenApi.ToSchema DeactivateAt where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @Text)

makePrisms ''DeactivateAt

newtype DeactivateUser =
  DeactivateUser
  { deactivateUserDeactivateAt :: DeactivateAt
  }
    deriving (Generic,  Show )
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct DeactivateUser)


makeLensesWith camelCaseFields ''DeactivateUser

--------------------------------------------------------------------------------
-- SAML sso --------------------------------------------------------------------
--------------------------------------------------------------------------------
newtype SamlLoginRequest =
  SamlLoginRequest
  { samlLoginRequestLocation :: Text
  } deriving (Generic)
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct SamlLoginRequest)


makeLensesWith camelCaseFields ''SamlLoginRequest

newtype SsoEnabled =
  SsoEnabled
  { ssoEnabledEnabled :: Bool
  }
    deriving (Generic)
    deriving (ToJSON, FromJSON, OpenApi.ToSchema)
      via (JSONStruct SsoEnabled)


makeLensesWith camelCaseFields ''SsoEnabled

data SamlResponse =
  SamlResponse
  { samlResponseBody       :: Text
  , samlResponseRelayState :: Maybe Text
  }
    deriving (Show, Generic)
    deriving (OpenApi.ToSchema) via (JSONStruct SamlResponse)

instance Form.FromForm SamlResponse where
  fromForm f =
    SamlResponse <$> Form.parseUnique "SAMLResponse" f
    <*> Form.parseMaybe "RelayState" f

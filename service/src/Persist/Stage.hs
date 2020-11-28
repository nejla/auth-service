-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Persist.Stage where

import qualified Data.Aeson           as Aeson
import           Data.Monoid
import qualified Data.Text            as Text
import qualified Data.UUID            as UUID
import           Database.Esqueleto   (SqlString)
import           Database.Persist.Sql
import           Database.Persist.TH

import           Types

derivePersistFieldJSON "Aeson.Value"
derivePersistFieldJSON "Aeson.Object"

instance PersistField UserID where
    toPersistValue = toPersistValue . UUID.toString . unUserID
    fromPersistValue = \x -> case x of
        PersistDbSpecific bs ->
            case UUID.fromASCIIBytes bs of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show bs)
             Just u -> Right $ UserID u
        PersistText txt ->
            case UUID.fromString $ Text.unpack txt of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show txt)
             Just u -> Right $ UserID u
        e -> Left $ "Can not convert to uuid: " <> (Text.pack $ show e)

instance PersistFieldSql UserID where
    sqlType _ = SqlOther "uuid"

instance PersistField InstanceID where
    toPersistValue = toPersistValue . UUID.toString . unInstanceID
    fromPersistValue = \x -> case x of
        PersistDbSpecific bs ->
            case UUID.fromASCIIBytes bs of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show bs)
             Just u -> Right $ InstanceID u
        PersistText txt ->
            case UUID.fromString $ Text.unpack txt of
             Nothing -> Left $ "Invalid UUID: " <> (Text.pack $ show txt)
             Just u -> Right $ InstanceID u
        e -> Left $ "Can not convert to uuid: " <> (Text.pack $ show e)

instance PersistFieldSql InstanceID where
    sqlType _ = SqlOther "uuid"

deriving instance PersistField Name
deriving instance PersistFieldSql Name

deriving instance PersistField Password
deriving instance PersistFieldSql Password

deriving instance PersistField PasswordHash
deriving instance PersistFieldSql PasswordHash

deriving instance PersistField Email
deriving instance PersistFieldSql Email

deriving instance PersistField Phone
deriving instance PersistFieldSql Phone

deriving instance PersistField B64Token
deriving instance PersistFieldSql B64Token

instance SqlString Name
instance SqlString Email

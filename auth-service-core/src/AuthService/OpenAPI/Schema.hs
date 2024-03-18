{-# language DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes#-}

-- Generator for OpenAPI schemata

module AuthService.OpenAPI.Schema where

import qualified Data.Aeson                   as Aeson
import qualified Data.Char                    as Char
import           Data.Data                    (Typeable, Proxy(..))
import           GHC.Generics
import qualified Data.OpenApi.Internal.Schema as OpenApi
import qualified Data.OpenApi.ParamSchema     as OpenApi

import           Helpers                      (dropPrefix)
import Data.Aeson (camelTo2)

-- Wrapper for `deriving via`
newtype JSONStruct a = JSONStruct a deriving Show

fieldLabelModifier :: forall a m f. (Generic a, Datatype m
                   , Rep a ~ M1 D m f)
  => String -> String
fieldLabelModifier =
  let (n, ns) = case datatypeName @m undefined of
        (c : cs) -> (c, cs)
        _ -> error "Empty datatypeName"
  in camelTo2 '_' . dropPrefix (Char.toLower n : ns)

-- This *requires* TypeApplications to be called, the `a` type parameter needs
-- to be passed explicitly
options
  :: forall a m f. (Generic a, Datatype m
                   , Rep a ~ M1 D m f)
  => Aeson.Options
options =
  Aeson.defaultOptions {Aeson.fieldLabelModifier = fieldLabelModifier @a }


instance (Generic a
         , Aeson.GToJSON' Aeson.Value Aeson.Zero f
         , Aeson.GToJSON' Aeson.Encoding Aeson.Zero f
         , Datatype m, Rep a ~ M1 D m f) => Aeson.ToJSON (JSONStruct a) where


  toJSON (JSONStruct x) = Aeson.genericToJSON (options @a) x
  toEncoding (JSONStruct x) = Aeson.genericToEncoding (options @a) x


instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)
         , Datatype m, Rep a ~ M1 D m f
         ) => Aeson.FromJSON (JSONStruct a) where
  parseJSON x = JSONStruct <$> Aeson.genericParseJSON (options @a) x

schemaOptions
  :: forall a m f. (Generic a, Datatype m
                   , Rep a ~ M1 D m f)
  => OpenApi.SchemaOptions
schemaOptions =
  let (n, ns) = case datatypeName @m undefined of
        (c : cs) -> (c, cs)
        _ -> error "Empty datatypeName"
      fieldLabelModifier = dropPrefix (Char.toLower n : ns)
   in  OpenApi.defaultSchemaOptions {OpenApi.fieldLabelModifier }

instance (Generic a, Typeable a
         , Datatype m, Rep a ~ M1 D m f
         , OpenApi.GToSchema (Rep a)
         )
         => OpenApi.ToSchema (JSONStruct a) where

  declareNamedSchema _ = OpenApi.genericDeclareNamedSchema options (Proxy @a)
    where
      options = OpenApi.defaultSchemaOptions { OpenApi.fieldLabelModifier
                                                = fieldLabelModifier @a
                                             }

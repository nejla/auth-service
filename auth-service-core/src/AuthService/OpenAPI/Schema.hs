{-# language DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes#-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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

data NamingStyle = CamelCase | Underscore
  deriving (Show, Eq, Typeable)

class NameStyleModifier (a :: NamingStyle) where
  modifier :: proxy a -> String -> String

instance NameStyleModifier 'CamelCase where
  modifier _ = Prelude.id

instance NameStyleModifier 'Underscore where
  modifier _ = camelTo2 '_'

-- Wrapper for `deriving via`
newtype JSONStructWith (p :: NamingStyle) a = JSONStruct a deriving Show

type JSONStruct = JSONStructWith 'CamelCase

fieldLabelModifier :: forall a m f. (Generic a, Datatype m
                   , Rep a ~ M1 D m f)
  => String -> String
fieldLabelModifier =
  let (n, ns) = case datatypeName @m undefined of
        (c : cs) -> (c, cs)
        _ -> error "Empty datatypeName"
  in dropPrefix (Char.toLower n : ns)

-- This *requires* TypeApplications to be called, the `a` type parameter needs
-- to be passed explicitly
options
  :: forall a m f. ( Generic a
                   , Datatype m
                   , Rep a ~ M1 D m f
                   )
  => (String -> String) -> Aeson.Options
options styleModifier =
  Aeson.defaultOptions {Aeson.fieldLabelModifier =
                           styleModifier . fieldLabelModifier @a }


instance ( Generic a
         , Aeson.GToJSON' Aeson.Value Aeson.Zero f
         , Aeson.GToJSON' Aeson.Encoding Aeson.Zero f
         , NameStyleModifier nsm
         , Datatype m, Rep a ~ M1 D m f)
         => Aeson.ToJSON (JSONStructWith nsm a) where


  toJSON (JSONStruct x) = Aeson.genericToJSON (options @a styleModifier) x
    where
      styleModifier = modifier (Proxy :: Proxy nsm)
  toEncoding (JSONStruct x) = Aeson.genericToEncoding (options @a styleModifier) x
    where
      styleModifier = modifier (Proxy :: Proxy nsm)


instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)
         , Datatype m, Rep a ~ M1 D m f
         , NameStyleModifier nsm
         ) => Aeson.FromJSON (JSONStructWith nsm a) where
  parseJSON x =
    let styleModifier = modifier (Proxy :: Proxy nsm)
    in JSONStruct <$> Aeson.genericParseJSON (options @a styleModifier) x

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

instance ( Generic a, Typeable a
         , Datatype m, Rep a ~ M1 D m f
         , OpenApi.GToSchema (Rep a)
         , NameStyleModifier nsm, Typeable nsm
         )
         => OpenApi.ToSchema (JSONStructWith nsm a) where

  declareNamedSchema _ = OpenApi.genericDeclareNamedSchema options (Proxy @a)
    where
      styleModifier = modifier (Proxy :: Proxy nsm)
      options = OpenApi.defaultSchemaOptions { OpenApi.fieldLabelModifier
                                                = styleModifier
                                                  . fieldLabelModifier @a
                                             }

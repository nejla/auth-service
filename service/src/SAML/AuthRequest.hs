{-# LANGUAGE OverloadedStrings #-}
module SAML.AuthRequest where

import           Codec.Compression.Zlib.Raw  as Deflate
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time.Clock             (getCurrentTime)
import           Data.Time.Format.ISO8601
import qualified Data.UUID                   as UUID
import qualified Data.UUID.V4                as UUID
import           Network.HTTP.Types          (urlEncode)
import           Text.XML

data AuthRequest = AuthRequest

samlpName :: Text -> Name
samlpName local =
  Name { nameLocalName = local
       , nameNamespace = Just "urn:oasis:names:tc:SAML:2.0:protocol"
       , namePrefix = Just "samlp"
       }
samlName :: Text -> Name
samlName local =
  Name { nameLocalName = local
       , nameNamespace = Just "urn:oasis:names:tc:SAML:2.0:assertion"
       , namePrefix = Just "samlp"
       }

-- [AuthnRequest]
authRequestElement :: Text -> Text -> Text -> Element
authRequestElement id' instant issuer =
  Element (samlpName "AuthnRequest")
     (Map.fromList
       [ ("ID", id') --  See [RequestAbstractType] and [ID Values]
       , ("Version", "2.0") -- See [RequestAbstractType]
       , ("IssueInstant", instant) -- See [RequestAbstractType]
       ]
     )
  [ NodeElement $ Element (samlpName "Issuer") mempty [NodeContent issuer]]


-- [HTTP redirect binding]
renderAuthRequest :: Text -> Text -> Text -> BSL.ByteString
renderAuthRequest id' instant issuer =
  renderLBS options $ Document (Prologue mempty Nothing mempty) element []
  where
    options = def { rsXMLDeclaration = False }
    element = authRequestElement id' instant issuer

-- [HTTP redirect binding]
mkRequest :: Text -> IO BS.ByteString
mkRequest issuer = do
  uuid <- UUID.nextRandom
  let id' = "ID-" <> UUID.toText uuid
  instant <- Text.pack  . iso8601Show <$> getCurrentTime
  let lbs = renderAuthRequest id' instant issuer
      compressed = urlEncode True . BSL.toStrict . Base64.encode $ Deflate.compress lbs

  return $ "SAMLRequest=" <> compressed

-- [RequestAbstractType]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=36
-- Section: 3.2.1 Complex Type RequestAbstractType

-- [AuthnRequest]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=48
-- Section: 3.4.1 Element <AuthnRequest>

-- [HTTP redirect binding]
-- Source:
-- https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf#page=15
-- Section: 3.4 HTTP Redirect Binding

-- [ID Values]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=9
-- Section: 1.3.4 ID and ID Reference Values
-- Note: ID Values must conform to "xs:ID", which in turn has a restriction of "xs:NCName" (non-colonized name).
-- In practice that means they are a string consisting of
-- 1. 1 of: Letter or '_'
-- then 0 or more of: Letter, Digit, '.', '-', '_',  CombiningChar, Extender
--
-- Definitions of character classes: https://www.w3.org/TR/2000/WD-xml-2e-20000814#CharClasses
-- Compare e.g. https://stackoverflow.com/questions/1631396/what-is-an-xsncname-type-and-when-should-it-be-used
--and https://www.w3.org/TR/xmlschema-2/#dt-ccesN (see \i and \c, bute not that colons are excluded)

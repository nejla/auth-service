{-# LANGUAGE OverloadedStrings #-}
module SAML.Keycloak where

import qualified Crypto.PubKey.RSA.Types  as RSA
import           Data.ByteString          (ByteString)
import           Data.Default
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as Text
import           Text.XML                 as XML
import           Text.XML.Cursor

import           SAML.Keys

import           Data.ASN1.Types
import           Data.ASN1.Encoding
import           Data.ASN1.BitArray
import qualified Data.ASN1.BinaryEncoding as ASN1


parseCertificateAsn1 :: [ASN1] -> Either String ByteString
parseCertificateAsn1
  [ Start Sequence
  , Start Sequence
  , IntVal _serial -- 1658151128088
  , Start Sequence
  , OID [1, 2, 840, 113549, 1, 1, 11]
  , Null
  , End Sequence
  , Start Sequence
  , Start Set
  , Start Sequence
  , OID [2, 5, 4, 3]
  , ASN1String _
  , End Sequence
  , End Set
  , End Sequence
  , Start Sequence
  , ASN1Time _ _created _
  , ASN1Time _ _validUntil _
  , End Sequence
  , Start Sequence
  , Start Set
  , Start Sequence
  , OID [2, 5, 4, 3]
  , ASN1String _
  , End Sequence
  , End Set
  , End Sequence
  , Start Sequence
  , Start Sequence
  , OID [1, 2, 840, 113549, 1, 1, 1]
  , Null
  , End Sequence
  , BitString (BitArray _length bs)
  , End Sequence
  , End Sequence
  , Start Sequence
  , OID [1, 2, 840, 113549, 1, 1, 11]
  , Null
  , End Sequence
  , BitString _
  , End Sequence] = Right bs
parseCertificateAsn1 _ = Left "certificate"


data SamlConfig =
  SamlConfig
  { signingPrivate    :: RSA.PrivateKey
  , signingPublic     :: RSA.PublicKey
  , encryptionPrivate :: RSA.PrivateKey
  , idpSSOUri         :: Text
  }

readConfig :: FilePath -> IO (Either String SamlConfig)
readConfig path = do
  doc <- XML.readFile def path
  let cursor = fromDocument doc
  return $ do -- Either String
    -- adapter <- oneElem cursor "keycloak-saml-adapter"
    sp <- oneElem cursor "SP"
    keys <- someElem sp "Keys"
    signing <- oneOr "signing key" $
                 keys >>= ($/ attributeIs "signing" "true")
    signingPrivateDer <-
      readDer . Text.encodeUtf8  =<<
      oneOr "signing private"
        (signing $/ (element "PrivateKeyPem" &/ content))
    signingPrivateKey <- parsePrivateKeyAsn1 signingPrivateDer
    signingCertDer <-
      readDer . Text.encodeUtf8  =<<
      oneOr "signing certificate"
        (signing $/ (element "CertificatePem" &/ content))
    signingPubKeyDer <- parseCertificateAsn1 signingCertDer
    signingPubKeyAsn1 <- case decodeASN1' ASN1.DER signingPubKeyDer of
                    Left e -> Left $ "Could not parse ASN.1: " ++ show e
                    Right r -> Right r
    signingPubKey <- parseRSAPubKeyASN1 signingPubKeyAsn1
    encryption <- oneOr "encryption" $
                  keys >>= ($/ attributeIs "encryption" "true")
    encryptionPrivateDer <-
      readDer . Text.encodeUtf8  =<<
      oneOr "encryption private"
        (encryption $/ (element "PrivateKeyPem" &/ content))
    encryptionPrivateKey <- parsePrivateKeyAsn1 encryptionPrivateDer
    idp <- oneElem sp "IDP"
    ssoService <- oneElem idp "SingleSignOnService"
    ssoServiceUri <- oneOr "bindingUrl" $ (ssoService $/ (attribute "bindingUrl"))
    return
      SamlConfig
      { signingPrivate = signingPrivateKey
      , signingPublic = signingPubKey
      , encryptionPrivate = encryptionPrivateKey
      , idpSSOUri = ssoServiceUri
      }
  where
    oneElem cursor name = oneOr (show name) $ cursor $/ (element name)
    someElem cursor name =
      case cursor $/ (element name) of
        [] -> Left $ show name
        r -> Right r

    oneOr _str [x] = Right x
    oneOr str  _ = Left str

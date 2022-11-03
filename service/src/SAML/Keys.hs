{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SAML.Keys where

import           Crypto.Number.Basic      (numBytes)
import qualified Crypto.PubKey.RSA.Types  as RSA
import qualified Data.ASN1.BinaryEncoding as ASN1
import           Data.ASN1.BitArray       (BitArray(..))
import qualified Data.ASN1.Encoding       as ASN1
import           Data.ASN1.Prim
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Char8    as BS8
import qualified Data.Char                as Char
import qualified Data.X509                as X509

--------------------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------
--------------------------------------------------------------------------------

trim :: ByteString -> ByteString
trim = BS8.dropWhile Char.isSpace . BS8.dropWhileEnd Char.isSpace

readPEM :: ByteString -> ByteString -> Either String ByteString
readPEM tag bs
  | trimmed <- trim bs
  , Just withoutPrefix <- BS.stripPrefix ("-----BEGIN " <> tag <> "-----") trimmed
  , Just withoutSuffix <- BS.stripSuffix ("-----END " <> tag <>"-----") withoutPrefix
     = Right $ BS8.filter (not . Char.isSpace) withoutSuffix
  | otherwise = Left "Could not read PEM"

readDer :: ByteString -> Either String [ASN1]
readDer b64 = do
  der <- Base64.decode $ trim b64
  case ASN1.decodeASN1' ASN1.DER der of
    Left e -> Left $ "Could not parse ASN.1: " ++ show e
    Right r -> Right r

readAsn1Der der =
    case ASN1.decodeASN1' ASN1.DER der of
      Left e -> Left $ "Could not parse ASN.1: " ++ show e
      Right r -> Right r


--------------------------------------------------------------------------------
-- Private Key -----------------------------------------------------------------
--------------------------------------------------------------------------------

-- https://datatracker.ietf.org/doc/html/rfc3447#appendix-A.1
-- RSAPrivateKey ::= SEQUENCE {
--     version           Version,
--     modulus           INTEGER,  -- n
--     publicExponent    INTEGER,  -- e
--     privateExponent   INTEGER,  -- d
--     prime1            INTEGER,  -- p
--     prime2            INTEGER,  -- q
--     exponent1         INTEGER,  -- d mod (p-1)
--     exponent2         INTEGER,  -- d mod (q-1)
--     coefficient       INTEGER,  -- (inverse of q) mod p
--     otherPrimeInfos   OtherPrimeInfos OPTIONAL
-- }
-- | Parse private key binary data from ASN1 abstract type
parsePrivateKeyAsn1 :: [ASN1] -> Either String RSA.PrivateKey
parsePrivateKeyAsn1
  [ Start Sequence
  , IntVal 0 -- version
  , IntVal n
  , IntVal e
  , IntVal d
  , IntVal p
  , IntVal q
  , IntVal dp
  , IntVal dq
  , IntVal qinv
  , End Sequence
  ] =
  let privatePub =
        RSA.PublicKey
        { public_size = numBytes n
        , public_n = n
        , public_e = e
        }
  in Right RSA.PrivateKey
     { private_pub = privatePub
     , private_d = d
     , private_p = p
     , private_q = q
     , private_dP = dp
     , private_dQ = dq
     , private_qinv = qinv
     }
parsePrivateKeyAsn1 _ = Left "Could not parse ASN.1 key data"


-- | Read Base64- DER encoded private key
parsePrivateKeyPem :: ByteString -> Either String RSA.PrivateKey
parsePrivateKeyPem pem = do
  b64 <- readPEM "RSA PRIVATE KEY" pem
  der <- readDer b64
  parsePrivateKeyAsn1 der

--------------------------------------------------------------------------------
-- Public Key ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Parse the X509-style public key envelope
parsePublicKeyASN1 :: [ASN1] -> Either String ByteString
parsePublicKeyASN1
  [ Start Sequence
  , Start Sequence
  , OID [1,2,840,113549,1,1,1]
  , Null
  , End Sequence
  , BitString (BitArray _length bs) -- actual content
  , End Sequence
  ] = Right bs
parsePublicKeyASN1 _ = Left "Could not parse ASN.1"

-- https://datatracker.ietf.org/doc/html/rfc3447#appendix-A.1.1
-- RSAPublicKey ::= SEQUENCE {
--       modulus           INTEGER,  -- n
--       publicExponent    INTEGER   -- e
--   }
parseRSAPubKeyASN1 :: [ASN1] -> Either String RSA.PublicKey
parseRSAPubKeyASN1
  [ Start Sequence
  , IntVal n
  , IntVal e
  , End Sequence
  ] = Right RSA.PublicKey
  { public_size = numBytes n
  , public_n = n
  , public_e = e
  }
parseRSAPubKeyASN1 _ = Left "Could not parse RSA public key"

parseRSAPubkey :: [ASN1] -> Either [Char] RSA.PublicKey
parseRSAPubkey bs = do -- Either String
  keyDer <- parsePublicKeyASN1 bs
  keyAsn1 <- case ASN1.decodeASN1' ASN1.DER keyDer of
    Left e -> Left $ "Could not parse ASN.1: " ++ show e
    Right r -> return r
  parseRSAPubKeyASN1 keyAsn1

parsePubkeyPem :: ByteString -> (Either String RSA.PublicKey)
parsePubkeyPem pem = do
  b64 <- readPEM "PUBLIC KEY" pem
  der <- readDer b64
  parseRSAPubkey der

--------------------------------------------------------------------------------
-- Certificate -----------------------------------------------------------------
--------------------------------------------------------------------------------

parseCertificatePem :: ByteString -> Either String RSA.PublicKey
parseCertificatePem pem = do
  b64 <- readPEM "CERTIFICATE" pem
  certBs <- Base64.decode b64
  cert <- X509.decodeSignedObject certBs
  case X509.certPubKey $ X509.signedObject $ X509.getSigned cert of
    X509.PubKeyRSA r -> return r
    _ -> Left "Expected RSA public key"

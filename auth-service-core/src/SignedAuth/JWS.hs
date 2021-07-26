{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module SignedAuth.JWS where

import           Control.Lens
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.TH              as Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy       as BSL
import           Data.Text                  (Text)
import           Data.Time.Clock.POSIX      (POSIXTime)
import           Data.Word

import           SignedAuth.Util
import           SignedAuth.Nonce
import           SignedAuth.Sign
import qualified Data.Char                  as Char

data Header = Header { headerAlg :: Text
                     , headerCrit :: [Text]
                     , headerT :: Word32
                     , headerN :: Nonce
                     } deriving (Show, Eq)

Aeson.deriveJSON (aesonTHOptions "header") ''Header
makeLensesWith camelCaseFields ''Header

-- JWS-signed bytestring
newtype JwsBs  = JwsBs ByteString

dot :: Word8
dot = fromIntegral (Char.ord '.')

-- | Sign a bytestring and prepend the signature
-- key, input => base64_url_unpadded(sign(input,key)) ++ "." ++ input
signed :: PrivateKey -> POSIXTime -> Nonce -> ByteString -> JwsBs
signed key now nonce payload =
  let header = Header { headerAlg = "ed25519"
                      , headerCrit = ["t", "n"]
                      , headerT = round now
                      , headerN = nonce
                      }
      signInput = BS.concat [ B64U.encodeUnpadded (BSL.toStrict $ Aeson.encode header)
                            , "."
                            , B64U.encodeUnpadded payload
                            ]

      Signature sig = sign key signInput
  in JwsBs $ BS.concat [signInput , "." , B64U.encodeUnpadded sig]

-- | Verify an octet sequence signed according to 'sign' and return the signed payload
verified :: PublicKey -> JwsBs -> Maybe (Header, ByteString)
verified pubKey (JwsBs signedBS)
  -- Split "header.payload.signature" into parts
  | (signInput, base64SigDot) <- BS.splitAt (BS.length signedBS
                                             - 86 {- Note [Signature-length] -}
                                             - 1  {- '.' -}) signedBS
  , (".", base64Sig) <- BS.splitAt 1 base64SigDot
  , [headerB64, payloadB64] <- BS.split (fromIntegral $ Char.ord '.') signInput
  -- parse header
  , Right headerBytes <- B64U.decodeUnpadded headerB64
  , Just header <- Aeson.decode' $ BSL.fromStrict headerBytes
  -- Verify signature
  , header ^. alg == "ed25519"
  , Right sig <- B64U.decodeUnpadded base64Sig
  , True <- verifySignature pubKey signInput (Signature sig)
  -- Check that we fully understand the header
  , header ^. crit == ["t" , "n"]
  -- Decode payload
  , Right payload <- B64U.decodeUnpadded payloadB64
    = Just (header, payload)
  | otherwise = Nothing

b64U :: Prism' ByteString ByteString
b64U = prism' B64U.encodeUnpadded (\bs -> case B64U.decodeUnpadded bs of
                                      Left _ -> Nothing
                                      Right r -> Just r
                                  )

-- Lenses
jwsParts :: Prism' JwsBs (ByteString, ByteString, ByteString)
jwsParts = prism' (\(header, payload, sig)
                   -> JwsBs $ BS.intercalate "." [header,payload, sig]
                 )
                 (\(JwsBs bs) -> case BS.split dot bs of
                                 [header, payload, sig]
                                   -> Just (header, payload, sig)
                                 _ -> Nothing
                 )

jwsHeader :: Traversal' JwsBs Header
jwsHeader = jwsParts . _1 . b64U . iso (\bs -> case Aeson.decode' (BSL.fromStrict bs) of
                                          Nothing -> error "could not decode header"
                                          Just r -> r
                                          )
                                (BSL.toStrict . Aeson.encode)

jwsPayload :: Traversal' JwsBs ByteString
jwsPayload = jwsParts . _2 . b64U

jwsSignature :: Traversal' JwsBs ByteString
jwsSignature = jwsParts . _3 . b64U

-- Note [Signature-length]
-- Base64: 3 octets become 4 base64 characters, padded to to full 4 characters
-- ED25519 produces 64 signature octets
-- 64 octets => /6*8 => 86 base64 characters + 2 paddings (dropped)

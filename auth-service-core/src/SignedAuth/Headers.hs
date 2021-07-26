{-# LANGUAGE OverloadedStrings #-}
module SignedAuth.Headers where

import           Control.Lens
import qualified Data.Aeson            as Aeson
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           SignedAuth.JWS
import           SignedAuth.Nonce
import           SignedAuth.Sign

-- JSON-encoded payload signed as JWS
newtype JWS a = JWS ByteString

encodeHeaders ::
  Aeson.ToJSON payload => PrivateKey -> NoncePool -> payload -> IO (JWS payload)
encodeHeaders key noncePool payload = do
  nonce <- mkNonce noncePool
  now <- getPOSIXTime
  let JwsBs bs = signed key now nonce (BSL.toStrict $ Aeson.encode payload)
  return $ JWS bs

decodeHeaders ::
  Aeson.FromJSON payload =>
     PublicKey
  -> Frame
  -> JWS payload
  -> IO (Either String payload)
decodeHeaders key nonceFrame (JWS sig) =
  case verified key (JwsBs sig) of
    Nothing -> return $ Left "signature rejected"
    Just (header, encoded) ->
      case Aeson.decode' $ BSL.fromStrict encoded of
        Nothing -> return $ Left "failed to parse payload"
        Just payload -> do
          verdict <- handleNonce nonceFrame (fromIntegral $ header ^. t) (header ^. n)
          case verdict of
            RejectSeen -> return $ Left "nonce rejected: already used"
            RejectOld -> return $ Left "nonce rejected: too old"
            Accept -> return $ Right payload

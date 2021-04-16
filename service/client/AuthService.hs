{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuthService
  ( login
  , logout
  , checkToken
  ) where

import qualified Control.Exception as Ex hiding (try)
import qualified Control.Monad.Catch as Ex
import           Data.Aeson
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Data
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Web.PathPieces

import           AuthService.Types

data AuthServiceException
    = AuthServiceHttpException HttpException
    | AuthServiceResponseException Status ResponseHeaders
    | AuthServiceResponseDecodeException Text
    deriving (Show, Typeable)

data LoginFailure = OTPRequired
                  | CredentialsRejected
                    deriving (Show, Eq)

instance Ex.Exception AuthServiceException

throwResponse :: Response r -> IO a
throwResponse response =
    Ex.throwM $ AuthServiceResponseException (responseStatus response) (responseHeaders response)


decodeJSON :: Aeson.FromJSON a => BSL.ByteString -> IO a
decodeJSON bs =
    case eitherDecode' bs of
     Left e -> Ex.throwM . AuthServiceResponseDecodeException
               $ Text.pack e
     Right r -> return r

call' :: ToJSON a =>
         ByteString
      -> String
      -> [Text]
      -> Maybe a
      -> IO (Either (Response BSL.ByteString) BSL.ByteString)
call' meth site urlPath v = do
    let body = maybe "" Aeson.encode v
    manager <- newManager tlsManagerSettings
    requestUrl <- parseRequest site
    let contentType = case meth of
                   "POST" | not (BSL.null body) ->
                            [ ("Content-Type", "application/json")]
                   _ -> []
        request = requestUrl { path = BS.intercalate "/"
                                        (Text.encodeUtf8 <$>urlPath)
                             , method = meth
                             , requestHeaders =
                                 contentType
                             -- , checkStatus = \_status _rhdrs _cookies -> Nothing
                             , requestBody = RequestBodyLBS body
                             }
    mbResponse <- Ex.try $ httpLbs request manager
    case mbResponse of
     Left (e :: HttpException) -> Ex.throwM $ AuthServiceHttpException e
     Right response ->
         case statusIsSuccessful $ responseStatus response of
          True -> return . Right $ responseBody response
          False -> return $ Left response

call :: ToJSON a =>
        ByteString
     -> String
     -> [Text]
     -> Maybe a
     -> IO BSL.ByteString
call meth site urlPath v = do
    res <- call' meth site urlPath v
    case res of
     Right r -> return r
     Left res' -> throwResponse res'

-- | Login using user credentials.
--
-- Returns (Left Credentialsrejected) when username or password don't match
--
-- Return (Left OTPRequired) when a One-time password is required. The OTP is
-- automaticallt created and sent to the user and should be transmitted in a new
-- request
--
-- Returns (Right token) when the login succeeds
login :: String -> Login -> IO (Either LoginFailure B64Token)
login site credentials = do
    res <- call' "POST" site ["login"] (Just credentials)
    case res of
     Right r -> Right <$> decodeJSON r
     Left response | statusCode (responseStatus response) == 499 ->
                         return $ Left OTPRequired
                   | statusCode (responseStatus response) == 403 ->
                         return $ Left CredentialsRejected
     Left response -> throwResponse response

-- | Logout a user given a token
logout :: String -> B64Token -> IO ()
logout site token = do
    _ <- call "POST" site ["logout", toPathPiece token] (Nothing :: Maybe ())
    return ()

-- | Check a token for validity.
--
-- Returns Nothing of the token is invalied
--
-- Returns (Just UserID) if the token is valid
checkToken :: String -- ^ Address of the auth service
           -> Text -- ^ Instance to verify
           -> B64Token -- ^ Token to verify
           -> IO (Maybe ReturnUser)
checkToken site inst token = do
    res <- call' "GET" site ["checkToken"
                            , inst
                            , toPathPiece token] (Nothing :: Maybe ())
    case res of
     Right r -> decodeJSON r
     Left response | statusCode (responseStatus response) == 403 -> return Nothing
     Left response -> throwResponse response

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Twilio where

import Control.Monad.Catch as Ex
import Control.Monad.Logger qualified as Logger
import Control.Monad.Trans
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Logging
import Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import Types

apiVersion :: BS.ByteString
apiVersion = "2010-04-01"

sendMessage
  :: (Logger.MonadLogger m, MonadCatch m, MonadIO m)
  => TwilioConfig
  -> Phone
  -> Text
  -> m ()
sendMessage
  TwilioConfig
    { twilioConfigAccount = account'
    , twilioConfigAuthToken = authToken'
    , twilioConfigSourceNumber = from
    }
  (Phone to)
  msg = do
    let accountSid = Text.encodeUtf8 account'
        username = accountSid
        password' = Text.encodeUtf8 authToken'
    manager <- liftIO $ newManager tlsManagerSettings
    request' <- liftIO $ parseRequest "https://api.twilio.com/"
    let urlPath =
          BS.intercalate
            "/"
            [ ""
            , apiVersion
            , "Accounts"
            , accountSid
            , "Messages"
            ]
        body =
          [ ("From", Text.encodeUtf8 from)
          , ("To", Text.encodeUtf8 to)
          , ("Body", Text.encodeUtf8 msg)
          ]
        req =
          urlEncodedBody body $
            request'
              { HTTP.path = urlPath
              , HTTP.requestHeaders =
                  [ mkAuth username password'
                  ]
              }
    mbResponse <- Ex.try $ httpLbs req manager
    case mbResponse of
      Left (e :: HttpException) -> do
        logError $ "Error while connection to Twilio: " <> showText e
        return ()
      Right response ->
        if statusIsSuccessful $ responseStatus response
          then return ()
          else do
            logError $
              "Twilio returned error response "
                <> showText (responseStatus response)
                <> "; "
                <> ( Text.decodeUtf8 . BSL.toStrict $
                      responseBody response
                   )

            return ()
   where
    showText :: (Show a) => a -> Text
    showText = Text.pack . show
    mkAuth username password' =
      ("Authorization", "Basic " <> B64.encode (username <> ":" <> password'))

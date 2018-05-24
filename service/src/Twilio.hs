-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Twilio where

import           Control.Monad.Catch    as Ex
import qualified Control.Monad.Logger   as Logger
import           Control.Monad.Trans
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy   as BSL
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Network.HTTP.Conduit   as HTTP
import           Network.HTTP.Types

import           Logging
import           Types

apiVersion :: BS.ByteString
apiVersion = "2010-04-01"

sendMessage ::
     (Logger.MonadLogger m, MonadCatch m, MonadIO m)
  => TwilioConfig
  -> Phone
  -> Text
  -> m ()
sendMessage TwilioConfig{ twilioConfigAccount = account'
                        , twilioConfigAuthToken = authToken'
                        , twilioConfigSourceNumber = from
                        } (Phone to) msg = do
    let accountSid = Text.encodeUtf8 account'
        username = accountSid
        password' = Text.encodeUtf8 authToken'
    manager <- liftIO $ newManager tlsManagerSettings
    request' <- liftIO $ parseRequest "https://api.twilio.com/"
    let urlPath = BS.intercalate "/" [ ""
                                     , apiVersion
                                     , "Accounts"
                                     , accountSid
                                     , "Messages"
                                     ]
        body = [ ("From", Text.encodeUtf8 from)
               , ("To", Text.encodeUtf8 to)
               , ("Body", Text.encodeUtf8 msg)
               ]
        req = urlEncodedBody body $
                    request'{ HTTP.path   = urlPath
                            , HTTP.requestHeaders = [mkAuth username password'
                                               ]
                            }
    mbResponse <- Ex.try $ httpLbs req manager
    case mbResponse of
      Left (e :: HttpException) -> do
          logError $ "Error while connection to Twilio: " <> showText e
          return ()
      Right response -> case statusIsSuccessful $ responseStatus response of
                True -> return  ()
                False -> do
                  logError $ "Twilio returned error response "
                                <> showText (responseStatus response)
                                <> "; " <> (Text.decodeUtf8 . BSL.toStrict $
                                             responseBody response )

                  return ()
  where
    showText :: Show a => a -> Text
    showText = Text.pack . show
    mkAuth username password'  =
        ("Authorization", "Basic " <> (B64.encode $ username <> ":" <> password'))

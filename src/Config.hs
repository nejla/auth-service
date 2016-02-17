-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Config where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.Environment
import qualified System.Exit as Exit

import           Helpers
import           Types

--------------------------------------------------------------------------------
-- Config file -----------------------------------------------------------------
--------------------------------------------------------------------------------

getConfGenericMaybe :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                       (String -> Maybe a)
                    -> String
                    -> Conf.Name
                    -> Conf.Config
                    -> m (Maybe a)
getConfGenericMaybe fromString env confName conf = do
    mbConfVal <- liftIO $ Conf.lookup conf confName
    case mbConfVal of
     Just v -> return $ Just v
     Nothing -> do
       mbVal <- liftIO $ lookupEnv env
       case mbVal of
        Nothing -> return Nothing
        Just v -> case fromString v of
                   Nothing -> do
                       $logError . Text.pack $
                            "Error reading environment variable \""
                            ++ env ++ "\": could not parse value " ++ show v
                       liftIO Exit.exitFailure
                   Just val -> return $ Just val

getConfGeneric :: (MonadLogger m, MonadIO m, Conf.Configured a) =>
                  (String -> Maybe a)
               -> String
               -> Conf.Name
               -> Either Text a
               -> Conf.Config
               -> m a
getConfGeneric fromString env confName mbDefault conf = do
    mbC <- getConfGenericMaybe fromString env confName conf
    case mbC of
     Nothing -> case mbDefault of
                 Right d -> return d
                 Left e -> do
                     $logError $ "Configuration of `"  <> e <> "` is required. \n"
                                 <> " Set environment variable "
                                 <> Text.pack env <>
                                 " or configuration variable " <> confName <> "."
                     liftIO $ Exit.exitFailure
     Just v -> return v

safeRead :: Read a => String -> Maybe a
safeRead str = case reads str of
     ((v,_):_) -> Just v
     _ -> Nothing

getConf' :: (Conf.Configured a, MonadLogger m, MonadIO m, Read a) =>
           String -> Conf.Name -> Either Text a -> Conf.Config -> m a
getConf' = getConfGeneric safeRead

getConfMaybe :: (MonadIO m, MonadLogger m) =>
                String
             -> Conf.Name
             -> Conf.Config
             -> m (Maybe Text)
getConfMaybe = getConfGenericMaybe (Just . Text.pack)

getConfMaybe' :: (Read a, MonadIO m, MonadLogger m, Conf.Configured a) =>
                 String
              -> Conf.Name
              -> Conf.Config
              -> m (Maybe a)
getConfMaybe' = getConfGenericMaybe safeRead

getConf :: (MonadLogger m, MonadIO m) =>
            String -> Conf.Name -> Either Text Text -> Conf.Config -> m Text
getConf = getConfGeneric (Just . Text.pack)

getConfBool :: (MonadIO m, MonadLogger m) =>
               String
            -> Conf.Name
            -> Either Text Bool
            -> Conf.Config
            -> m Bool
getConfBool = getConfGeneric parseBool
  where
    parseBool str | (map toLower $ str) == "true" = Just True
                  | (map toLower $ str) == "false" = Just False
                  | otherwise = Nothing

loadConf :: MonadIO m => m Conf.Config
loadConf = liftIO $ do
    mbConfPath <- lookupEnv "CONF_PATH"
    let confFiles =  catMaybes [ Just $ Conf.Optional "/data/auth_service.conf"
                               , Conf.Required <$> mbConfPath
                               ]
    debug $ "Loading conf files " <> showText confFiles
    Conf.load confFiles

--------------------------------------------------------------------------------
-- Configuration ---------------------------------------------------------------
--------------------------------------------------------------------------------

getDBString :: (MonadLogger m, MonadIO m) => Conf.Config -> m ByteString
getDBString conf = do
    host <- getConf "DB_HOST" "db.host" (Right "database") conf
    usr <- getConf "DB_USER" "db.user" (Right "postgres") conf
    db   <- getConf "DB_DATABASE" "db.database" (Right "postgres")
                    conf
    pwd <- getConf "DB_PASSWORD" "db.password"
                        (Right "") conf
    return . BS.intercalate " "
        $ [ "host"     .= host
          , "user"     .= usr
          , "dbname"   .= db
          , "password" .= pwd
          ]
  where
    _ .= "" = ""
    k .= v = k <> "=" <> (Text.encodeUtf8 v)

getTwilioConfig :: (MonadIO m, MonadLogger m) =>
                   Conf.Config
                -> m (Maybe TwilioConfig)
getTwilioConfig conf = do
    mbAccount <- getConfMaybe "TWILIO_ACCOUNT" "twilio.account" conf
    mbAuthToken <- getConfMaybe "TWILIO_TOKEN" "twilio.token" conf
    mbSourceNumber <- getConfMaybe "TWILIO_SOURCE" "twilio.source" conf
    case (mbAccount, mbAuthToken, mbSourceNumber) of
        (Nothing, Nothing, Nothing) -> return Nothing
        (Just account, Just authToken, Just sourceNumber) ->
            return $ Just TwilioConfig { twilioConfigAccount = account
                                       , twilioConfigAuthToken = authToken
                                       , twilioConfigSourceNumber = sourceNumber
                                       }
        _ -> do
            $logError "Twilio config is incomplete"
            liftIO Exit.exitFailure

get2FAConf conf = do
     tfaRequired <- getConfBool "TFA_REQUIRED" "tfa.required" (Right False) conf
     twilioConf <- getTwilioConfig conf
     case (tfaRequired, twilioConf) of
      (True, Nothing) -> do
          $logError "Two Factor Authentication is required, but Twilio is not configured"
          liftIO Exit.exitFailure
      _ -> return (tfaRequired, twilioConf)


getAuthServiceConfig :: (MonadIO m, MonadLogger m) =>
                        Conf.Config
                     -> m Config
getAuthServiceConfig conf = do
    timeout <- getConf' "TOKEN_TIMEOUT" "token.timeout"
                 (Right 3600) {- 1 hour -} conf
    dbString <- getDBString conf
    otpl <- getConf' "OTP_LENGTH" "otp.length"
                 (Right 4) conf
    otpt <- getConf' "OTP_TIMEOUT" "otp.timeout"
                 (Right 300) conf
    (tfaRequired, twilioConf) <- get2FAConf conf
    return Config{ configTimeout = timeout
                 , configDbString = dbString
                 , configOTPLength = otpl
                 , configOTPTimeoutSeconds = otpt
                 , configTFARequired = tfaRequired
                 , configTwilio = twilioConf
                 }

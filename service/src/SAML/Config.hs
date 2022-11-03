{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module SAML.Config where

import           Control.Monad.Logger             (logError)
import           Control.Monad.Trans
import qualified Data.ByteString                  as BS
import           Data.Default                     (def)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes)
import           Data.String.Interpolate.IsString (i)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.IO                     as Text
import qualified Data.Traversable                 as Traversable
import qualified Data.UUID                        as UUID
import qualified System.Directory                 as Dir
import           System.Exit                      (exitFailure)
import           System.FilePath                  ((</>))

import           NejlaCommon.Config               hiding (Config)

import           SAML.Keys

import           Types


readConfigLines :: Text -> Map Text Text
readConfigLines txt =
  Map.fromList
    [ (Text.strip k, Text.strip v)
    | line <- Text.lines txt -- Ignore lines that only contain white spaces
    , (k,v) <- splitEq line
    ]
  where
    splitEq :: Text -> [(Text, Text)]
    -- Split on the first equals sign, removing it. Additional equals signs are
    -- considered part of the value
    splitEq line = let (k, suff) = Text.breakOn "=" line
                  in case Text.stripPrefix "=" suff of
                       Nothing -> []
                       Just v -> [(k, v)]

readConfigFile :: MonadIO m => FilePath -> m (Map Text Text)
readConfigFile path = liftIO $ do
  txt <- Text.readFile path
  return $ readConfigLines txt

getSamlConfig base inst = do
  let path = base </> inst
  let configurationPath = path </> "config"

  -- Private key
  let privateKeyPath = path </> "key.pem"
  privKeyBs <- liftIO $ BS.readFile privateKeyPath
  samlInstanceConfigEncryptionKey <- case parsePrivateKeyPem privKeyBs of
    Left e -> do
      $logError $ "Could not parse SAML private key: " <> Text.pack e
      liftIO exitFailure
    Right r -> return r

  -- Realm Certificate
  let certPath = path </> "certificate.pem"
  certBs <- liftIO $ BS.readFile certPath
  samlInstanceConfigSigningKey <- case parseCertificatePem certBs of
    Left e -> do
      $logError $ "Could not parse SAML certificate: " <> Text.pack e
      liftIO exitFailure
    Right r -> return r

  -- config file
  let conffilePath = path </> "config"
  conf <- readConfigFile conffilePath
  samlInstanceConfigAudience <- get "audience" conf
  let samlInstanceConfigRedirectAfterLogin = Map.lookup "redirect_after_login" conf
  samlInstanceConfigInstance <- get "instance" conf >>= \instTxt ->
    case UUID.fromText instTxt of
      Nothing -> do
        $logError [i|SAML: Could not read instance ID #{instTxt} for folder #{inst} as an UUID|]
        liftIO exitFailure
      Just iid -> return $ InstanceID iid
  samlInstanceConfigIdPBaseUrl <- get "idp_request_url" conf

  return SamlInstanceConfig{..}
  where
    get k m = case Map.lookup k m of
                  Just v -> return v
                  Nothing -> do
                    $logError [i|SAML: missing configuration entry #{k} for folder ${inst}|]
                    liftIO exitFailure

getConfigs basePath = do
  entries <- liftIO $ Dir.getDirectoryContents basePath
  configs <- Traversable.forM entries $ \dir ->
    if dir `elem` [".", ".."] then return Nothing
    else do
      isDir <- liftIO $ Dir.doesDirectoryExist (basePath </> dir)
      if isDir then do
          conf <- getSamlConfig basePath dir
          return $ Just (samlInstanceConfigInstance conf, conf)
        else return Nothing
  return $ catMaybes configs

getSAMLConfig conf = do
  mbSamlConfPath <- getConfMaybe "SAML_CONFIG_PATH"
    "saml.configPath" conf
  case mbSamlConfPath of
    Nothing -> return Nothing
    Just path -> do
      configs <- getConfigs $ Text.unpack path
      return (Just SamlConfig{samlConfigInstances = Map.fromList configs})

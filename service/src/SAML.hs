{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SAML
  ( module SAML
  , readConfig
  )

where

import           Control.Monad.Except
import           Control.Monad.Logger             (MonadLogger)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base64           as B64
import           Data.Functor
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString (i)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Time.Clock                  (getCurrentTime, addUTCTime)
import qualified Data.UUID                        as UUID
import qualified Database.Persist                 as P
import qualified Network.Wai.SAML2                as SAML
import qualified Network.Wai.SAML2.Validation     as SAML

import           SAML.Keycloak                    (readConfig)
import qualified SAML.Keycloak                    as Keycloak
import           SAML.Keys
import           SAML.AuthRequest                 as AuthRequest

import           AuthService.Types

import           Audit
import           Backend
import           Types
import qualified Logging                          as Log
import           Monad
import qualified Persist.Schema                   as DB

--------------------------------------------------------------------------------
-- SAML config -----------------------------------------------------------------
--------------------------------------------------------------------------------

mkSamlConfig :: ByteString -> ByteString -> Either String SAML.SAML2Config
mkSamlConfig privkeyPem pubkeyPem = do -- Either String
  privateKey <- parsePrivateKeyPem privkeyPem
  publicKey <- parsePubkeyPem pubkeyPem
  return $ SAML.saml2Config privateKey publicKey
keycloakConf2SamlConf :: Keycloak.SamlConfig -> SAML.SAML2Config

keycloakConf2SamlConf cfg = do
   SAML.saml2Config (Keycloak.encryptionPrivate cfg)
                    (Keycloak.signingPublic cfg)

config2SamlConf :: SamlInstanceConfig -> SAML.SAML2Config
config2SamlConf cfg =
  SAML.saml2Config (samlInstanceConfigEncryptionKey cfg)
    (samlInstanceConfigSigningKey cfg)

data SSOResult =
  SSOInvalid
  deriving (Show)

createSsoToken
  :: Text
  -> Email
  -> Name
  -> InstanceID
  -> Text
  -> [Text]
  -> API ReturnLogin
createSsoToken userId email userName instId instName roles = do
  now <- liftIO getCurrentTime
  -- We set the absolute expiration time for the token here, but the
  -- expiration timeout for tokens that haven't been used in a while is
  -- re-calculated on each check
  mbTokenExpiration <- getConfig timeout
  let tokenExpires = mbTokenExpiration <&> \texp ->
        -- fromInteger on NominalDiffTime assumes seconds
        fromInteger texp `addUTCTime` now
  token' <- B64Token . ("sso:" <> )
              <$> mkRandomString tokenChars 22 -- > 128 bit
  _key <-
    runDB . P.insert $
    DB.SsoToken
    { DB.ssoTokenToken = token'
    , DB.ssoTokenUserId = userId
    , DB.ssoTokenEmail = email
    , DB.ssoTokenName = userName
    , DB.ssoTokenInstanceId = instId
    , DB.ssoTokenCreated = now
    , DB.ssoTokenExpires = tokenExpires
    , DB.ssoTokenLastUse = Nothing
    , DB.ssoTokenDeactivated = Nothing
    }

  runDB $ P.insertMany_
    [ DB.SsoTokenRole
      { DB.ssoTokenRoleToken = token'
      , DB.ssoTokenRoleRole = role
      }
    | role <- roles ]

  audit AuditSsoTokenCreated
        { auditSsoUserID = userId
        , auditCreatedToken = unB64Token token'
        }
  return
    ( ReturnLogin
      {returnLoginToken = token', returnLoginInstances =
                                      [ReturnInstance instName instId]})

checkCondition :: ( MonadLogger m, MonadError SSOResult m
                  , Show a, Show b
                  ) =>
                  Text -> a -> b -> Bool -> m ()
checkCondition _txt _a _b True = return ()
checkCondition txt a b False = do
  Log.logInfo $ [i|SAML condition not met: #{txt}; SAML condition prescribes: #{a} but we have #{b}|]
  throwError SSOInvalid

ssoAssertHandler :: Text
  -> InstanceID
  -> SAML.SAML2Config
  -> SamlResponse
  -> API (Either SSOResult ReturnLogin)
ssoAssertHandler audience defaultInstance cfg response = runExceptT $ do
  res <- liftIO (SAML.validateResponse cfg (Text.encodeUtf8 $ samlResponseBody response))
         >>= \case
           Left e -> do
             lift . Log.logInfo $ "Failed validating SAML assertion: "
                                    <> (Text.pack $ show e)
             lift . Log.logDebug $
               (either (const "could not decode Base64") Text.decodeUtf8 . B64.decode . Text.encodeUtf8 $ samlResponseBody response)
             throwError $ SSOInvalid
           Right r -> do
             liftIO $ print r
             return r
  lift $ storeAssertionID (SAML.assertionId res) -- this can throw conflict
  -- check conditions
  now <- liftIO getCurrentTime

  let cond = SAML.assertionConditions res
  checkCondition "not before" (SAML.conditionsNotBefore cond) now
    $ SAML.conditionsNotBefore cond <= now
  checkCondition "not on or after" (SAML.conditionsNotOnOrAfter cond) now
    $ SAML.conditionsNotOnOrAfter cond > now
  checkCondition "audience" (SAML.conditionsAudience cond) audience
    $ audience == SAML.conditionsAudience cond

  let attrs = Map.fromListWith (++)
                 [(SAML.attributeName attr, [SAML.attributeValue attr])
                 | attr <- SAML.assertionAttributeStatement res
                 ]
      -- attributes we care about

  let getAttrs name =  do
        case Map.lookup name attrs of
          Nothing ->
            return []
          Just val -> do
            Log.logDebug $ "SAML attribute found: " <> name <> " = "
              <> (Text.pack $ show val)
            return val
      getAttr name = do
        getAttrs name >>= \case
          [] -> do
            Log.logInfo [i|Missing SAML attribute: #{name}|]
            throwError SSOInvalid
          [x] -> return x
          (x:y:_) -> do
            Log.logInfo [i|Duplicate SAML attribute: #{name}|]
            throwError SSOInvalid

  email <- getAttr "email"
  userName <- getAttr "name"
  let userId = SAML.nameIdValue . SAML.subjectNameId $ SAML.assertionSubject res
  role <- getAttrs "role"
  instanceId <-
    case Map.lookup "instanceId" attrs of
      Nothing -> return defaultInstance
      Just [instTxt] | Just inst <- UUID.fromText instTxt -> return $ InstanceID inst
                     | otherwise -> do
                         Log.logInfo $ "Could not parse SAML instance id "
                             <> instTxt
                         throwError SSOInvalid
      Just{} -> do
        Log.logInfo "Multiple SAML instances are unsupported"
        throwError SSOInvalid
  Log.logDebug $ "instanceId "  <> (Text.pack $ show instanceId)

  mbInstance <- lift $ getInstance instanceId
  case mbInstance of
    Nothing -> do
      Log.logInfo $ "Unknown instance " <> (Text.pack $ show instanceId)
      throwError SSOInvalid
    Just inst -> do
      lift
        $ createSsoToken (userId) (Email email) (Name userName) instanceId
            (DB.instanceName inst) role

-- TODO: Should we check ID and issuer?
ssoLoginHandler :: Text -> IO ByteString
ssoLoginHandler = do
  mkRequest

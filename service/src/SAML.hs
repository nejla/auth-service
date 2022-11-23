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
import qualified Database.Esqueleto               as E
import qualified Database.Persist                 as P
import           NejlaCommon                      (SV)
import qualified Network.Wai.SAML2                as SAML
import qualified Network.Wai.SAML2.Validation     as SAML

import qualified Network.Wai.SAML2.Request        as AuthnRequest
import           SAML.Keycloak                    (readConfig)
import qualified SAML.Keycloak                    as Keycloak
import           SAML.Keys

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
  (SAML.saml2Config (samlInstanceConfigEncryptionKey cfg)
                   (samlInstanceConfigSigningKey cfg))
                   { SAML.saml2RequireEncryptedAssertion =
                       not (samlInstanceConfigAllowUnencrypted cfg)
                   , SAML.saml2Audiences = [ samlInstanceConfigAudience cfg ]
                   }

--------------------------------------------------------------------------------
-- AuthnRequest ----------------------------------------------------------------
--------------------------------------------------------------------------------

ssoLoginHandler :: Text -> API ByteString
ssoLoginHandler issuer = do
  request <- liftIO $ AuthnRequest.issueAuthnRequest issuer

  now <- liftIO getCurrentTime
  _ <- runDB $ P.insert DB.SamlRequestId
    { DB.samlRequestIdRequestId = AuthnRequest.authnRequestID request
    , DB.samlRequestIdCreated = now
    }

  return $ "SAMLRequest=" <> AuthnRequest.renderUrlEncodingDeflate request



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
createSsoToken userId email' userName instId instName roles' = do
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
    , DB.ssoTokenEmail = email'
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
    | role <- roles' ]

  audit AuditSsoTokenCreated
        { auditSsoUserID = userId
        , auditCreatedToken = unB64Token token'
        }
  return
    ( ReturnLogin
      {returnLoginToken = token', returnLoginInstances =
                                      [ReturnInstance instName instId]})

checkAssertionInResponseTo :: Text -> ExceptT SSOResult API ()
checkAssertionInResponseTo requestId = do
  now <- liftIO getCurrentTime
  -- Delete IDs older than one hour
  lift . runDB . E.delete . E.from $ \(srid :: SV DB.SamlRequestId) ->
     E.where_ (srid E.^. DB.SamlRequestIdCreated E.<. E.val (addUTCTime (-3600) now))

  -- Find and delete request ID
  deleted <- lift . runDB $ E.deleteCount . E.from $ \(srid :: SV DB.SamlRequestId) ->
     E.where_ (srid E.^. DB.SamlRequestIdRequestId E.==. E.val requestId)
  unless (deleted > 0) $  do
     Log.logError [i|Failed validating SAML assertion: InResponseTo with unknown request ID|]
     throwError SSOInvalid

ssoAssertHandler
  :: SamlInstanceConfig
  -> SamlResponse
  -> API (Either SSOResult ReturnLogin)
ssoAssertHandler cfg response = runExceptT $ do
  (assertion, mbInResponseTo) <- liftIO (SAML.validateResponse
                                      (SAML.config2SamlConf cfg)
                                      (Text.encodeUtf8 $ samlResponseBody response))
         >>= \case
           Left e -> do
             lift . Log.logInfo $ "Failed validating SAML assertion: "
                                    <> Text.pack (show e)
             lift . Log.logDebug $
               (either (const "could not decode Base64") Text.decodeUtf8 . B64.decode . Text.encodeUtf8 $ samlResponseBody response)
             throwError SSOInvalid
           Right r -> do
             liftIO $ print r
             return r

  -- Reference [InResponseTo]
  --
  case mbInResponseTo of
    Nothing -> unless (samlInstanceConfigAllowUnsolicited cfg) $ do
      Log.logError [i|Failed validating SAML assertion: InResponseTo missing but we don't allow unsolicited responses|]
      throwError SSOInvalid
    Just inResponseTo -> checkAssertionInResponseTo inResponseTo

  lift $ storeAssertionID (SAML.assertionId assertion) -- this can throw conflict

  let attrs = Map.fromListWith (++)
                 [(SAML.attributeName attr, [SAML.attributeValue attr])
                 | attr <- SAML.assertionAttributeStatement assertion
                 ]

  -- attributes we care about
  let getAttrs name' =  do
        case Map.lookup name' attrs of
          Nothing ->
            return []
          Just val -> do
            Log.logDebug $ "SAML attribute found: " <> name' <> " = "
              <> Text.pack (show val)
            return val
      getAttr name' = do
        getAttrs name' >>= \case
          [] -> do
            Log.logInfo [i|Missing SAML attribute: #{name'}|]
            throwError SSOInvalid
          [x] -> return x
          (_:_:_) -> do
            Log.logInfo [i|Duplicate SAML attribute: #{name'}|]
            throwError SSOInvalid

  email <- getAttr "email"
  userName <- getAttr "name"
  let userId = SAML.nameIDValue . SAML.subjectNameID $ SAML.assertionSubject assertion
  role <- getAttrs "role"
  instanceId <-
    case Map.lookup "instanceId" attrs of
      Nothing -> return $ samlInstanceConfigInstance cfg
      Just [instTxt] | Just inst <- UUID.fromText instTxt -> return $ InstanceID inst
                     | otherwise -> do
                         Log.logInfo $ "Could not parse SAML instance id "
                             <> instTxt
                         throwError SSOInvalid
      Just{} -> do
        Log.logInfo "Multiple SAML instances are unsupported"
        throwError SSOInvalid
  Log.logDebug $ "instanceId "  <> Text.pack (show instanceId)

  mbInstance <- lift $ getInstance instanceId
  case mbInstance of
    Nothing -> do
      Log.logInfo $ "Unknown instance " <> Text.pack (show instanceId)
      throwError SSOInvalid
    Just inst -> do
      lift
        $ createSsoToken userId (Email email) (Name userName) instanceId
            (DB.instanceName inst) role

-- Reference [InResponseTo]
-- > If the response is not generated in response to a request, or if the ID
-- > attribute value of a request cannot be determined (for example, the request
-- > is malformed), then this attribute MUST NOT be present.  Otherwise, it MUST
-- > be present and its value MUST match the value of the corresponding
-- > request's ID attribute.
-- Source:
-- https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=38
-- Section:
-- 3.2.2 Complex Type StatusResponseType

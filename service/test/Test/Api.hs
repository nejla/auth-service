{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import                          Control.Lens
import                          Control.Monad
import                          Data.ByteString                  (ByteString)
import qualified                Data.ByteString.Lazy             as BSL
import                          Data.Data                        (Proxy(..))
import                          Data.String.Interpolate.IsString (i)
import                          Data.Text                        (Text)
import qualified                Data.Text                        as Text
import qualified                Data.Text.Encoding               as Text
import                          Data.Time.Clock                  (getCurrentTime)
import                          Data.UUID                        (UUID)
import qualified                Data.UUID                        as UUID
import qualified                SignedAuth
import                          Test.Hspec
import                          Test.Hspec.Core.Spec
import                          Test.Hspec.Wai
import                          Test.Hspec.Wai.JSON

import                          Network.Wai                      (Application)
import                          Network.Wai.Test                 (SResponse)

import qualified "auth-service" Api
import                          Audit                            (AuditSource(AuditSourceTest))
import                          Backend
import                          Monad
import                          Test.Common
import                          Types

import                          Control.Concurrent               (threadDelay)
import                          NejlaCommon.Test                 (postJ)
import qualified                NejlaCommon.Test                 as NC

iid :: UUID
iid = case UUID.fromString "3afe62f4-7235-4b86-a418-923aaa4a5c28" of
        Just uuid -> uuid
        Nothing -> error "uuid"

runTest :: SpecWith ((), (Config -> Config) -> Application) -> IO ()
runTest spec = withTestDB $ \pool -> do
  hspec $ flip around spec $ \f -> do
    (conf, secrets) <- mkConfig pool
    noncePool <- SignedAuth.newNoncePool
    let apiState = ApiState { apiStateConfig = conf
                            , apiStateAuditSource = AuditSourceTest
                            , apiStateNoncePool = noncePool
                            }
    _ <- runAPI pool apiState $ do
      createUser adminUser
      addInstance (Just $ InstanceID iid) "instance1"
    f ((), \cc -> Api.serveAPI pool noncePool (cc conf) secrets)
  where
    adminUser = AddUser { addUserUuid      = Nothing
                        , addUserEmail     = "admin@example.com"
                        , addUserPassword  = "pwd"
                        , addUserName      = "admin"
                        , addUserPhone     = Nothing
                        , addUserInstances = []
                        , addUserRoles     = ["admin"]
                        }

data WithConfig st = WithConfig (Config -> Config) (WaiExpectation st)

withDefaultConfig :: WaiExpectation st -> WithConfig st
withDefaultConfig = WithConfig Prelude.id

instance Example (WithConfig st) where
  type Arg (WithConfig st) = (st, (Config -> Config) -> Application)
  evaluateExample (WithConfig wc m) p action  = do
    evaluateExample m p (\f -> action (\(st, wca) -> f (st, wca wc)))


main :: IO ()
main = runTest spec

spec :: SpecWith ((), (Config -> Config) -> Application)
spec = do
  describe "admin API" adminApiSpec
  describe "create account API" createAccountSpec
  describe "rate limiting" rateLimitSpec


exampleUser :: Text -> [Text] -> BSL.ByteString
exampleUser name roles =
                     [json|{ "name": #{name}
                           , "email" : #{name <> "@example.com"}
                           , "password" : "pwd"
                           , "instances" : [#{iid}]
                           , "roles": #{roles}
                           } |]

addUser :: Text -> Text -> [Text] -> WaiSession st UUID
addUser token name roles = do
  res <- postToken token "/admin/users" (exampleUser name roles)
    `NC.shouldReturnA` (Proxy @Types.ReturnUser)
  return $ res ^. user . _UserID

loginReq :: Text -> Text -> WaiSession st Text
loginReq username password = do
  res <- postJ [i|/login|] [json|{ "user": #{username <> "@example.com"}
                                 , "password": #{password}
                                }|] `NC.shouldReturnA` (Proxy @Types.ReturnLogin)
  return $ res ^. token . _B64Token

withAdminToken :: (Text -> WaiSession st b) -> WaiSession st b
withAdminToken f = do
  token <- loginReq "admin" "pwd"
  f token

withRegularToken :: (Text -> WaiSession st b) -> WaiSession st b
withRegularToken f = withAdminToken $ \adm -> do
  _ <- addUser adm "user" []
  tok <- loginReq "user" "pwd"
  f tok

postToken :: Text -> ByteString -> BSL.ByteString -> WaiSession st SResponse
postToken token path body =
  request "POST" path [ ("Content-Type", "application/json")
                      , ("X-Token", Text.encodeUtf8 token)
                      ] body

getToken :: Text -> ByteString -> WaiSession st SResponse
getToken token path = request "GET" path [("X-Token", Text.encodeUtf8 token)] ""


createAccountSpec :: SpecWith ((), (Config -> Config) -> Application)
createAccountSpec = do
  describe "/create-account" $ do
    it "Allows users to create an account" $ WithConfig Prelude.id $ do
      postJ "/create-account" [json| { "email": "create-account@example.com"
                                     , "password": "cac"
                                     , "name": "create account"
                                     , "phone": null
                                     } |] `shouldRespondWith` 201
      _ <- loginReq "create-account" "cac"
      return ()
    it "Disallows users to create an account when account creation is disabled"
      $ WithConfig (accountCreation . enabled .~ False) $ do
        postJ "/create-account" [json| { "email": "create-account@example.com"
                                       , "password": "cac"
                                       , "name": "create account"
                                       , "phone": null
                                       } |] `shouldRespondWith` 403

data TestRequestMethod = GET | POST deriving (Show, Eq, Ord)

data TestRequest = TR { trMethod :: TestRequestMethod
                      , trPath :: ByteString
                      -- We don't want to set a body, but unfortunately servant
                      -- tries to parse the request before we get a chance to
                      -- validate the token
                      , trBody :: BSL.ByteString
                      } deriving (Show)

method :: TestRequestMethod -> ByteString
method trm = case trm of
                GET -> "GET"
                POST -> "POST"


runRequest :: Maybe Text -> TestRequest -> WaiSession st SResponse
runRequest mbToken tr =
  let ct = case trMethod tr of
        -- All our POST requests send json data
        POST | not (BSL.null $ trBody tr)  -> [("Content-Type", "application/json")]
             | otherwise -> []
        GET -> []
      th = case mbToken of
             Nothing -> []
             Just token -> [("X-Token", Text.encodeUtf8 token)]
  in request (method $ trMethod tr) (trPath tr) (ct ++ th) (trBody tr)

describeRequest :: TestRequest -> Text
describeRequest tr = Text.decodeUtf8 $ method (trMethod tr) <> " " <> trPath tr

-- List of endpoints that should be limited to admin users
adminEndpoints :: [TestRequest]
adminEndpoints =
  [TR POST "/admin/users" (exampleUser "" [])
  ,TR GET  "/admin/users" ""
  -- We use iid as a dummy UUID, the point is to check that the endpoint is
  -- properly guarded before the existence of the user is checked
  ,TR POST [i|/admin/users/#{iid}/deactivate|] [i|{"deactivate_at": "now"}|]
  ,TR POST [i|/admin/users/#{iid}/reactivate|] ""
  ]


adminApiSpec :: SpecWith ((), (Config -> Config) -> Application)
adminApiSpec = do
  describe "endpoint authentication" $ do
    forM_ adminEndpoints  $ \endpoint -> do
      describe (Text.unpack $ describeRequest endpoint) $ do
        it "checks that token is set" $ withDefaultConfig $
          runRequest Nothing endpoint `shouldRespondWith` 403
        it "checks that user is admin" $ withDefaultConfig
          $ withRegularToken $ \token -> do
            runRequest (Just token) endpoint `shouldRespondWith` 403

  describe "/admin/users" $ do
    describe "POST" $ do
      it "succeeds" $ withDefaultConfig $ withAdminToken $ \admin -> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                            , "email" : "no@spam.please"
                                            , "password" : "pwd"
                                            , "instances" : [#{iid}]
                                            , "roles": []
                                            } |] `shouldRespondWith` 200

      it "allows created user to login"
       $ withDefaultConfig $ withAdminToken $ \admin -> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 200
        -- Check if new user can login
        postToken admin "/login" [json| { "user": "no@spam.please"
                              , "password": "pwd"
                              } |] `shouldRespondWith` 200

      it "disallows duplicate email addresses"
       $ withDefaultConfig $ withAdminToken $ \admin-> do
        postToken admin "/admin/users" [json|{ "name": "robert"
                                            , "email" : "no@spam.please"
                                            , "password" : "pwd"
                                            , "instances" : [#{iid}]
                                            , "roles": []
                                            } |] `shouldRespondWith` 200
        postToken admin "/admin/users" [json|{ "name": "bla"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   , "roles": []
                                   } |] `shouldRespondWith` 409

    describe "GET" $ do
      it "returns the list of users"
       $ withDefaultConfig  $ withAdminToken $ \admin-> do
        _ <- addUser admin "peter" []
        _ <- addUser admin "robert" []
        res <- getToken admin "/admin/users"
                `NC.shouldReturnA` (Proxy @[Types.ReturnUserInfo])
        -- "admin@example.com" is always added since we need admin privileges to
        -- run admin endpoints
        res ^.. each . email `NC.shouldBe` [ "admin@example.com"
                                           , "peter@example.com"
                                           , "robert@example.com"]

  describe "/admin/users/<uid>/deactivate" $ do
    describe "now" $ do
      it "prevents a user from logging in"
       $ withDefaultConfig $ withAdminToken $ \admin -> do
        uid <- addUser admin "robert" []
        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                          , "password": "pwd"
                                          }|] `shouldRespondWith` 200

        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                          , "password": "pwd"
                                          }|] `shouldRespondWith` 403
      it "disables existing tokens"
       $ withDefaultConfig $ withAdminToken $ \admin-> do
        uid <- addUser admin "robert" []

        tok <- loginReq "robert" "pwd"

        request "GET" [i|/check-token/|] [ ("X-Token", Text.encodeUtf8 tok)
                                         , ("X-Instance", UUID.toASCIIBytes iid)
                                         ] "" `shouldRespondWith` 200

        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": "now"}|]
              `shouldRespondWith` 204

        request "GET" [i|/check-token/|] [ ("X-Token", Text.encodeUtf8 tok)
                                         , ("X-Instance", UUID.toASCIIBytes iid)
                                         ] "" `shouldRespondWith` 403

    describe "time" $ do
      it "prevents a user from logging in"
       $ withDefaultConfig $ withAdminToken $ \admin-> do
        uid <- addUser admin "robert" []
        postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 200


        now <- liftIO getCurrentTime
        postToken admin [i|/admin/users/#{uid}/deactivate|]
                        [json|{"deactivate_at": #{now}}|]
              `shouldRespondWith` 204

        postToken admin [i|/login|]
                        [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 403

    it "doesn't prevent user from logging in when time is in the future"
     $ withDefaultConfig $ withAdminToken $ \admin-> do
      uid <- addUser admin "robert" []

      -- The year 2400 seems like a safe choice here
      postToken admin [i|/admin/users/#{uid}/deactivate|]
            [json|{"deactivate_at": "2400-08-22T11:36:31.973155386Z" }|]
            `shouldRespondWith` 204

      postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200
  describe "/admin/users/<uid>/reactivate" $ do
    it "Allows a user to log in again"
     $ withDefaultConfig $ withAdminToken $ \admin-> do
      uid <- addUser admin "robert" []
      postToken admin [i|/admin/users/#{uid}/deactivate|]
                      [json|{"deactivate_at": "now"}|]
            `shouldRespondWith` 204

      postToken admin [i|/admin/users/#{uid}/reactivate|] [json|{}|]
            `shouldRespondWith` 204

      postToken admin [i|/login|] [json|{ "user": "robert@example.com"
                              , "password": "pwd"
                              }|] `shouldRespondWith` 200

rateLimitSpec :: SpecWith ((), (Config -> Config) -> Application)
rateLimitSpec =
  describe "/login" $ do
    it "Limits login attempts" $
      WithConfig (  (maxAttempts .~ 1)
                  . (attemptsTimeframe .~ 1)
                  ) $ do
        postJ [i|/login|] [json|{ "user": "user@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 403
        postJ [i|/login|] [json|{ "user": "user@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 429

    it "Also limits correct credentials" $
      WithConfig (  (maxAttempts .~ 1)
                  . (attemptsTimeframe .~ 1)
                  ) $ do
        postJ [i|/login|] [json|{ "user": "admin@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 403
        postJ [i|/login|] [json|{ "user": "admin@example.com"
                                , "password": "pwd"
                                }|] `shouldRespondWith` 429

    it "Does not count successful attempts" $
      WithConfig (  (maxAttempts .~ 1)
                  . (attemptsTimeframe .~ 1)
                  ) $ do
        replicateM_ 5 $ loginReq "admin" "pwd"

    it "Allows more login attempts after the time frame runs out" $
      WithConfig (  (maxAttempts .~ 1)
                  . (attemptsTimeframe .~ 0.5)
                  ) $ do
        postJ [i|/login|] [json|{ "user": "user@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 403
        postJ [i|/login|] [json|{ "user": "user@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 429

        liftIO $ threadDelay 500_000
        postJ [i|/login|] [json|{ "user": "user@example.com"
                                , "password": "false"
                                }|] `shouldRespondWith` 403

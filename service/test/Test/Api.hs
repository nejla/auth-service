{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where


import           Data.UUID           (UUID)
import qualified Data.UUID           as UUID
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.Wai         (Application)

import qualified Api
import           Backend
import           Test.Common
import           Types

import           NejlaCommon.Test

iid :: UUID
Just iid = UUID.fromString "3afe62f4-7235-4b86-a418-923aaa4a5c28"

runTest :: SpecWith Application -> IO ()
runTest f = withApiData Nothing $ \pool conf -> do
  _ <- runAPI pool conf $ addInstance (Just $ InstanceID iid) "instance1"
  hspec $ with (return $ Api.serveAPI pool conf) f

main :: IO ()
main = runTest spec

spec :: SpecWith Application
spec = do
  describe "admin API" adminApiSpec

adminApiSpec :: SpecWith Application
adminApiSpec = do
  describe "/admin/users" $ do
    describe "POST" $ do
      it "returns 200" $ do
        postJ "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   } |] `shouldRespondWith` 200
      it "allows the user to login" $ do
        postJ "/admin/users" [json|{ "name": "robert"
                                   , "email" : "no@spam.please"
                                   , "password" : "pwd"
                                   , "instances" : [#{iid}]
                                   } |] `shouldRespondWith` 200
        -- Check if new user can login
        postJ "/login" [json| { "user": "no@spam.please"
                              , "password": "pwd"
                              } |] `shouldRespondWith` 200

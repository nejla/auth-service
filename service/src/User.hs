{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE OverloadedStrings #-}

module User where

import qualified Control.Monad.Catch as Ex
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           System.IO

import           Backend
import qualified Persist.Schema      as DB
import           System.Exit
import           Types

addUser :: [Text] -> API (Maybe UserID)
addUser args = do
  case args of
   (emailAddr : pwd : name' : mbPhone) -> do
       createUser AddUser{ addUserUuid     = Nothing
                         , addUserEmail    = Email emailAddr
                         , addUserPassword = Password pwd
                         , addUserName     = Name name'
                         , addUserPhone    = Phone <$> listToMaybe mbPhone
                         , addUserInstances = []
                         }
   _ -> liftIO $ do
       hPutStrLn stderr
           "Usage: auth-service adduser <email> <password> <name> [<phone>]"
       exitFailure


fetchUser :: Email -> API DB.User
fetchUser userEmail = do
  mbUser <- getUserByEmail userEmail
  case mbUser of
   Nothing -> do
     liftIO $ hPutStrLn stderr "User not found"
     liftIO exitFailure
   Just usr -> return usr

changePassword :: [String] -> API ()
changePassword args = do
  case Text.pack <$> args of
    [userEmail, pwd] -> do
      usr <- fetchUser (Email userEmail)
      res <- Ex.try $ changeUserPassword (DB.userUuid usr) (Password pwd)
      case res of
        Left (e :: ChangePasswordError) ->
          liftIO $ do
            hPutStrLn stderr "chpass: Could not create password hash"
            exitFailure
        _ -> return ()
    _ ->
      liftIO $ do
        hPutStrLn stderr "Usage: auth-service chpass <email> <password>"
        exitFailure

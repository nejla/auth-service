-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE OverloadedStrings #-}

module User where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.List as List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.IO

import           Backend
import qualified Persist.Schema as DB
import           System.Exit
import           Types

addUser :: [Text] -> API (Maybe ())
addUser args = do
  case args of
   (emailAddr : pwd : name : mbPhone) -> do
       createUser AddUser{ addUserEmail    = Email emailAddr
                         , addUserPassword = Password pwd
                         , addUserName     = Name name
                         , addUserPhone    = Phone <$> listToMaybe mbPhone
                         }
   _ -> liftIO $ do
       hPutStrLn stderr
           "Usage: auth-service adduser <email> <password> <name> [<phone>]"
       exitFailure


getUser :: Email -> API DB.User
getUser userEmail = do
  mbUser <- getUserByEmail userEmail
  case mbUser of
   Nothing -> do
     liftIO $ hPutStrLn stderr "User not found"
     liftIO exitFailure
   Just user -> return user

changePassword :: [String] -> API ()
changePassword args = do
  case Text.pack <$> args of
   [userEmail, pwd] -> do
     user <- getUser (Email userEmail)
     res <- changeUserPassword (DB.userUuid user) (Password pwd)
     case res of
      Nothing -> liftIO $ do
          hPutStrLn stderr "chpass: Could not create password hash"
          exitFailure
      _ -> return ()
   _ -> liftIO $ do
         hPutStrLn stderr
             "Usage: auth-service chpass <email> <password>"
         exitFailure

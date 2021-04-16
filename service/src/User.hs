{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module User where

import qualified Control.Monad.Catch      as Ex
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Time                (UTCTime)
import qualified Data.Time.Format.ISO8601 as ISO8601
import           Data.UUID                (UUID)
import qualified Data.UUID                as UUID
import           System.IO

import           Backend
import           Monad
import qualified Persist.Schema           as DB
import           System.Exit
import           Types

readTime :: String -> IO UTCTime
readTime str = case ISO8601.iso8601ParseM str of
                 Nothing -> do
                   hPutStrLn stderr $ "could not read time string \"" ++ str
                                       ++ "\". Is it in ISO8601 format?\n"
                                       ++ "Example: 2020-08-25T00:00:00Z"
                   exitFailure
                 Just time -> return time

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
                         , addUserRoles = []
                         }
   _ -> liftIO $ do
       hPutStrLn stderr
           "Usage: auth-service adduser <email> <password> <name> [<phone>]"
       exitFailure

addRole :: [String] -> API ()
addRole args = do
  case Text.pack <$> args of
    [emailAddr, role] -> do
      usr <- fetchUser (Email emailAddr)
      userAddRole (DB.userUuid usr) role
    _ -> liftIO $ do
      hPutStrLn stderr "Usage: auth-service addrole <email> <role>"
      exitFailure

removeRole :: [String] -> API ()
removeRole args = do
  case Text.pack <$> args of
    [emailAddr, role] -> do
      usr <- fetchUser (Email emailAddr)
      userRemoveRole (DB.userUuid usr) role
    _ -> liftIO $ do
      hPutStrLn stderr "Usage: auth-service rmrole <email> <role>"
      exitFailure

parseUUID :: Text -> API UUID
parseUUID text =
  case UUID.fromText text of
    Nothing -> liftIO $ do
      hPutStrLn stderr $
        "Could not parse UUID " <> show text
      exitFailure
    Just r -> return r

addInstance' :: [String] -> API ()
addInstance' args = do
  case Text.pack <$> args of
    [name] ->
      addInstance Nothing name >>= liftIO . print
    [name, uuidTxt] -> do
      uuid <- parseUUID uuidTxt
      addInstance (Just $ InstanceID uuid) name >>= liftIO . print
    _ -> liftIO $ do
      hPutStrLn stderr $ "Usage: auth-service newinstance <name> [<uuid>]"
      exitFailure

userAddInstance :: [String] -> API ()
userAddInstance args = do
  case Text.pack <$> args of
    [email, inst] -> do
      usr <- fetchUser (Email email)
      instUuid <- parseUUID inst
      addUserInstance (DB.userUuid usr) (InstanceID instUuid)
    _ -> liftIO $ do
      hPutStrLn stderr "Usage: auth-service addinstance <email> <uuid>"
      exitFailure

userRemoveInstance :: [String] -> API ()
userRemoveInstance args = do
  case Text.pack <$> args of
    [email, inst] -> do
      usr <- fetchUser (Email email)
      instUuid <- parseUUID inst
      n <- removeUserInstance (DB.userUuid usr) (InstanceID instUuid)
      if n > 0
        then return ()
        else liftIO $ do
               hPutStrLn stderr $
                 "User did not have access to instance " <> (Text.unpack inst)
               exitFailure
    _ -> liftIO $ do
      hPutStrLn stderr "Usage: auth-service removeinstance <email> <uuid>"

userDeactivate' :: [String] -> API ()
userDeactivate' [userEmail] = do
  usr <- fetchUser (Email $ Text.pack userEmail)
  deactivateUser (DB.userUuid usr) DeactivateNow
userDeactivate' [userEmail, deactivateAt] = do
  usr <- fetchUser (Email $ Text.pack userEmail)
  time <- liftIO $ readTime deactivateAt
  deactivateUser (DB.userUuid usr) (DeactivateAt time)
userDeactivate' _ = liftIO $ do
      hPutStrLn stderr "Usage: auth-service deactivateuser <email> [time]"
      exitFailure

userReactivate :: [String] -> API ()
userReactivate [userEmail] = do
  usr <- fetchUser (Email $ Text.pack userEmail)
  reactivateUser (DB.userUuid usr)
userReactivate _ = liftIO $ do
      hPutStrLn stderr "Usage: auth-service reactivateuser <email>"
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
      res <- Ex.try $ changeUserPassword Nothing (DB.userUuid usr) (Password pwd)
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

-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Backend
  (module Backend
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import qualified Crypto.BCrypt as BCrypt
import           Data.Maybe (listToMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock
import qualified Data.Traversable as Traversable
import qualified Data.UUID.V4 as UUID
import qualified Database.Esqueleto as E
import           Database.Esqueleto hiding ((^.), from)
import qualified Database.Persist as P
import qualified Database.Persist.Sql as P
import           System.Random
import qualified Twilio

import qualified Persist.Schema as DB
import           Types
import           Logging

for = flip fmap

showText :: Show a => a -> Text
showText = Text.pack . show

policy :: BCrypt.HashingPolicy
policy = BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 10 }

hashPassword :: Password -> API (Maybe PasswordHash)
hashPassword (Password pwd) = liftIO $
    fmap PasswordHash <$>
      BCrypt.hashPasswordUsingPolicy policy (Text.encodeUtf8 pwd)

createUser :: AddUser -> API (Maybe ())
createUser usr = do
    uid <- UserID <$> liftIO UUID.nextRandom
    mbHash <- hashPassword $ usr ^. password
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
       let dbUser = DB.User { DB.userUuid = uid
                            , DB.userName = usr ^. name
                            , DB.userPasswordHash = hash
                            , DB.userEmail = usr ^. email
                            , DB.userPhone = usr ^. phone
                            }
       _ <- runDB $ P.insert dbUser
       return $ Just ()

getUserByEmail :: Email -> API (Maybe DB.User)
getUserByEmail name = do
    fmap (fmap entityVal) . runDB $ P.getBy (DB.UniqueUserEmail name)

changeUserPassword :: UserID -> Password -> API (Maybe ())
changeUserPassword user password = do
    mbHash <- hashPassword password
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
         updates <- runDB $ P.updateWhere [DB.UserUuid P.==. user]
                                          [DB.UserPasswordHash P.=. hash]
         return $ Just updates

addUserInstance :: UserID -> InstanceID -> API ()
addUserInstance user inst = do
  _ <- runDB $ P.insert DB.UserInstance{ DB.userInstanceUser = user
                                       , DB.userInstanceInstanceId = inst
                                       }
  return ()

removeUserInstance :: UserID -> InstanceID -> API Integer
removeUserInstance user inst = do
  count <- runDB $ P.deleteWhereCount [ DB.UserInstanceUser P.==. user
                                      , DB.UserInstanceInstanceId P.==. inst
                                      ]
  return $ fromIntegral count

getUserInstances :: UserID -> API [ReturnInstance]
getUserInstances user = do
  res <- runDB . select . E.from $ \(ui `InnerJoin` i) -> do
     on (i E.^. DB.InstanceUuid ==. ui E.^. DB.UserInstanceInstanceId)
     where_ (ui E.^. DB.UserInstanceUser ==. val user)
     orderBy [asc $ i E.^. DB.InstanceName ]
     return i
  return $ for (entityVal <$> res) $ \i ->
              ReturnInstance{ returnInstanceName = i ^. name
                            , returnInstanceId = i ^. DB.uuid
                            }

otpIDChars :: [Char]
otpIDChars = "CDFGHJKLMNPQRSTVWXYZ2345679"

mkRandomString chars len =
    liftIO $ Text.pack <$> replicateM len (selectOne chars)
  where
    selectOne xs = do
        i <- randomRIO (0, length xs - 1)
        return $ xs !! i

mkRandomOTP ::  API Password
mkRandomOTP = do
    len <- getConfig oTPLength
    Password <$> mkRandomString otpIDChars len


sendOTP :: TwilioConfig -> Email -> Phone -> Password -> API ()
sendOTP twilioConf (Email user) (Phone p) (Password otp) = do
    logInfo $ mconcat [ "Sending OTP for user " , user
                       , "(", p , ")"
                       , ": " <> otp
                       ]
    Twilio.sendMessage (twilioConf ^. account)
                       (twilioConf ^. authToken)
                       (twilioConf ^. sourceNumber)
                       p
                       otp

tokenChars :: [Char]
tokenChars = concat [ ['a' .. 'z']
                    , ['A' .. 'Z']
                    , ['0' .. '9']
                    ] -- Roughly 6 bit per char


login :: Login -> API (Either LoginError ReturnLogin)
login Login{ loginUser = userEmail
           , loginPassword = pwd
           , loginOtp      = mbOtp
           } = do
    mbUser <- runDB $ P.getBy (DB.UniqueUserEmail userEmail)
    case mbUser of
     Nothing -> return $ Left LoginErrorFailed
     Just (Entity _ usr) -> do
         let hash = usr ^. DB.passwordHash
             userId = usr ^. DB.uuid
         case checkPassword hash pwd of
           True -> do
               unless (hashUsesPolicy hash) $ rehash userId
               twilioConf <- getConfig twilio
               case mbOtp of
                Nothing -> do
                    mbTwilioConf <- getConfig twilio
                    case mbTwilioConf of
                     Nothing -> Right <$> createToken userId
                     Just twilioConf ->
                         case DB.userPhone usr of
                          -- No phone number for two-factor auth
                          Nothing -> Right <$> createToken userId
                          Just p  -> do
                              createOTP twilioConf p userId
                              return $ Left LoginErrorOTPRequired
                Just (Password otpC) -> do
                    let otp = Password $ Text.toUpper otpC
                    otpTime <- fromIntegral . negate <$> getConfig oTPTimeoutSeconds
                    now <- liftIO getCurrentTime
                    let cutoff = otpTime `addUTCTime` now
                    runDB $ P.deleteWhere [DB.UserOtpCreated P.<=. cutoff]
                    checkOTP <- runDB $ P.selectList [ DB.UserOtpUser P.==. userId
                                                     , DB.UserOtpPassword P.==. otp
                                                     ] []
                    case checkOTP of
                     (_:_) -> do
                         runDB $ P.deleteWhere [ DB.UserOtpUser P.==. userId
                                               , DB.UserOtpPassword P.==. otp
                                               ]
                         Right <$> createToken userId
                     [] -> return $ Left LoginErrorFailed
           False -> return $ Left LoginErrorFailed
  where
    rehash userId = do
        mbNewHash <- hashPassword pwd
        case mbNewHash of
         Nothing -> return () -- TODO: log error
         Just newHash -> runDB $ P.update (DB.UserKey userId)
                                          [DB.UserPasswordHash P.=. newHash]
    createToken userId = do
        now <- liftIO $ getCurrentTime
        -- token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
        token <- B64Token <$> mkRandomString tokenChars 22 -- > 128 bit
        _ <- runDB . P.insert $ DB.Token { DB.tokenToken = token
                                         , DB.tokenUser = userId
                                         , DB.tokenCreated = now
                                         , DB.tokenExpires = Nothing
                                         }
        instances <- getUserInstances userId
        return ReturnLogin{ returnLoginToken = token
                          , returnLoginInstances = instances
                          }
    checkPassword (PasswordHash hash) (Password pwd') =
        BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) =
        BCrypt.hashUsesPolicy policy hash
    createOTP twilioConf p userId = do
        otp <- mkRandomOTP
        now <- liftIO getCurrentTime
        _ <- runDB $ insert DB.UserOtp { DB.userOtpUser = userId
                                       , DB.userOtpPassword = otp
                                       , DB.userOtpCreated = now
                                       }
        sendOTP twilioConf userEmail p otp
        return ()

getUserByToken :: B64Token -> API (Maybe DB.User)
getUserByToken tokenId = do
  -- Delete expired tokens
  now <- liftIO $ getCurrentTime
  runDB $ P.deleteWhere [DB.TokenExpires P.<=. Just now]

  user <- runDB . select . E.from $ \(user `InnerJoin` token) -> do
    on (user E.^. DB.UserUuid ==. token E.^. DB.TokenUser)
    where_ (token E.^. DB.TokenToken ==. val tokenId)
    return user
  return . fmap entityVal $ listToMaybe user

getUserInfo :: B64Token -> API (Maybe ReturnUserInfo)
getUserInfo token = do
  mbUser <- getUserByToken token
  Traversable.forM mbUser $ \user -> do
    instances <- getUserInstances (user ^. DB.uuid)
    return ReturnUserInfo { returnUserInfoId = user ^. DB.uuid
                          , returnUserInfoEmail = user ^. email
                          , returnUserInfoName = user ^. name
                          , returnUserInfoPhone = user ^. phone
                          , returnUserInfoInstances = instances
                          }

checkToken :: B64Token -> API (Maybe UserID)
checkToken tokenId = fmap DB.userUuid <$> getUserByToken tokenId

checkInstance  :: InstanceID -> UserID -> API (Maybe DB.UserInstance)
checkInstance inst user = runDB $ P.get (DB.UserInstanceKey user inst)

-- checkInstance :: InstanceID ->

logOut :: B64Token -> API ()
logOut token = do
    runDB $ P.delete (DB.TokenKey token)

-- addUser name password email mbPhone = do

-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Backend
  (module Backend
  ) where

import           Control.Arrow ((***))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
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

import           NejlaCommon (whereL)

import qualified Logging as Log
import qualified Persist.Schema as DB
import           Types

unOtpKey :: Key DB.UserOtp -> Log.OtpRef
unOtpKey = P.unSqlBackendKey . DB.unUserOtpKey

unTokenKey :: Key DB.Token -> Log.TokenRef
unTokenKey = P.unSqlBackendKey . DB.unTokenKey

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

showText :: Show a => a -> Text
showText = Text.pack . show

policy :: BCrypt.HashingPolicy
policy = BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 10 }

hashPassword :: Password -> API (Maybe PasswordHash)
hashPassword (Password pwd) = liftIO $
    fmap PasswordHash <$>
      BCrypt.hashPasswordUsingPolicy policy (Text.encodeUtf8 pwd)

createUser :: AddUser -> API (Maybe UserID)
createUser usr = do
    uid <- case usr ^. uuid of
             Nothing -> UserID <$> liftIO UUID.nextRandom
             Just u -> return u
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
       Log.logES $ Log.UserCreated{ Log.user = usr ^. email}
       _ <- runDB . P.insertMany $ for (usr ^. instances) $ \iid ->
           DB.UserInstance { DB.userInstanceUser = uid
                           , DB.userInstanceInstanceId = iid
                           }
       return $ Just uid

getUserByEmail :: Email -> API (Maybe DB.User)
getUserByEmail name' = do
    fmap (fmap entityVal) . runDB $ P.getBy (DB.UniqueUserEmail name')

changeUserPassword :: UserID -> Password -> API (Maybe ())
changeUserPassword user' password' = do
    mbHash <- hashPassword password'
    case mbHash of
     Nothing -> return Nothing
     Just hash -> do
         updates <- runDB $ P.updateWhere [DB.UserUuid P.==. user']
                                          [DB.UserPasswordHash P.=. hash]
         return $ Just updates

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------


addInstance :: Maybe InstanceID -> Text -> API InstanceID
addInstance mbIid name' = do
  instanceID <- case mbIid of
                  Nothing -> liftIO $ InstanceID <$> UUID.nextRandom
                  Just iid -> return iid
  _ <- runDB . P.insert $ DB.Instance{ DB.instanceUuid = instanceID
                                     , DB.instanceName = name'
                                     }
  return instanceID

addUserInstance :: UserID -> InstanceID -> API ()
addUserInstance user' inst = do
  _ <- runDB $ P.insert DB.UserInstance{ DB.userInstanceUser = user'
                                       , DB.userInstanceInstanceId = inst
                                       }
  return ()

removeUserInstance :: UserID -> InstanceID -> API Integer
removeUserInstance user' inst = do
  count' <- runDB $ P.deleteWhereCount [ DB.UserInstanceUser P.==. user'
                                       , DB.UserInstanceInstanceId P.==. inst
                                       ]
  return $ fromIntegral count'

getUserInstances :: UserID -> API [ReturnInstance]
getUserInstances user' = do
  res <- runDB . select . E.from $ \(ui `InnerJoin` i) -> do
     on (i E.^. DB.InstanceUuid ==. ui E.^. DB.UserInstanceInstanceId)
     where_ (ui E.^. DB.UserInstanceUser ==. val user')
     orderBy [asc $ i E.^. DB.InstanceName ]
     return i
  return $ for (entityVal <$> res) $ \i ->
              ReturnInstance{ returnInstanceName = i ^. name
                            , returnInstanceId = i ^. DB.uuid
                            }

otpIDChars :: [Char]
otpIDChars = "CDFGHJKLMNPQRSTVWXYZ2345679"

mkRandomString :: MonadIO m => [Char] -> Int -> m Text
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
sendOTP twilioConf (Email user') (Phone p) (Password otp') = do
    Log.logInfo $ mconcat [ "Sending OTP for user " , user'
                          , "(", p , ")"
                          , ": " <> otp'
                          ]
    Twilio.sendMessage (twilioConf ^. account)
                       (twilioConf ^. authToken)
                       (twilioConf ^. sourceNumber)
                       p
                       otp'

tokenChars :: [Char]
tokenChars = concat [ ['a' .. 'z']
                    , ['A' .. 'Z']
                    , ['0' .. '9']
                    ] -- Roughly 6 bit per char

checkUserPassword :: Email -> Password -> API (Either LoginError DB.User)
checkUserPassword userEmail pwd = do
    mbUser <- runDB $ P.getBy (DB.UniqueUserEmail userEmail)
    case mbUser of
     Nothing -> return $ Left LoginErrorFailed
     Just (Entity _ usr) -> do
         let hash = usr ^. DB.passwordHash
             userId = usr ^. DB.uuid
         case checkPassword hash pwd of
           True -> do
               unless (hashUsesPolicy hash) $ rehash userId
               return $ Right usr
           False -> return $ Left LoginErrorFailed
  where
    checkPassword (PasswordHash hash) (Password pwd') =
        BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) =
        BCrypt.hashUsesPolicy policy hash
    rehash userId = do
        mbNewHash <- hashPassword pwd
        case mbNewHash of
         Nothing -> return () -- TODO: log error
         Just newHash -> runDB $ P.update (DB.UserKey userId)
                                          [DB.UserPasswordHash P.=. newHash]

deactivateOtpWhere :: [P.Filter DB.UserOtp] -> API ()
deactivateOtpWhere selector = do
  now <- liftIO getCurrentTime
  runDB $ P.updateWhere selector [DB.UserOtpDeactivated P.=. Just now]

deactivateTokenWhere :: [P.Filter DB.Token] -> API ()
deactivateTokenWhere selector = do
  now <- liftIO getCurrentTime
  runDB $ P.updateWhere selector [DB.TokenDeactivated P.=. Just now]

login :: Login -> API (Either LoginError ReturnLogin)
login Login{ loginUser = userEmail
           , loginPassword = pwd
           , loginOtp      = mbOtp
           } = do
    mbError <- checkUserPassword userEmail pwd
    case mbError of
     Left e -> do
       Log.logES $ Log.AuthFailed{ Log.user = userEmail
                                 , Log.reason =
                                     Log.AuthFailedReasonWrongPassword
                                 }
       return $ Left e
     Right usr -> do
         let userId = usr ^. DB.uuid
         case mbOtp of
          Nothing -> do
              mbTwilioConf <- getConfig twilio
              case mbTwilioConf of
               Nothing -> do
                 (tid, rl) <- createToken userId
                 Log.logES Log.AuthSuccess{ Log.user = userEmail
                                          , Log.tokenId = tid
                                          }
                 return $ Right rl
               Just twilioConf ->
                   case DB.userPhone usr of
                    -- No phone number for two-factor auth
                    Nothing -> do
                      (tid, rl) <- createToken userId
                      Log.logES Log.AuthSuccess{ Log.user = userEmail
                                               , Log.tokenId = tid
                                               }
                      return $ Right rl
                    Just p  -> do
                        createOTP twilioConf p userId
                        return $ Left LoginErrorOTPRequired
          Just (Password otpC) -> do
              let otp' = Password $ Text.toUpper otpC
              otpTime <- fromIntegral . negate <$> getConfig oTPTimeoutSeconds
              now <- liftIO getCurrentTime
              let cutoff = otpTime `addUTCTime` now
              deactivateOtpWhere [DB.UserOtpCreated P.<=. cutoff]
              checkOTP <- runDB $ P.selectList [ DB.UserOtpUser P.==. userId
                                               , DB.UserOtpPassword P.==. otp'
                                               , DB.UserOtpDeactivated
                                                 P.==. Nothing
                                               ] []
              case checkOTP of
               (k:_) -> do
                   deactivateOtpWhere [ DB.UserOtpUser P.==. userId
                                      , DB.UserOtpPassword P.==. otp'
                                      ]
                   (tid,  rl) <- createToken userId
                   Log.logES Log.AuthSuccessOTP
                     { Log.user = userEmail
                     , Log.tokenId = tid
                     , Log.otp = P.unSqlBackendKey . DB.unUserOtpKey
                                   $ entityKey k
                     }
                   return $ Right rl
               [] -> do
                 Log.logES $ Log.AuthFailed{ Log.user = userEmail
                                           , Log.reason =
                                               Log.AuthFailedReasonWrongOtp
                                           }
                 return $ Left LoginErrorFailed
  where
    createToken userId = do
        now <- liftIO $ getCurrentTime
        -- token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
        token' <- B64Token <$> mkRandomString tokenChars 22 -- > 128 bit
        key <- runDB . P.insert $ DB.Token { DB.tokenToken = token'
                                           , DB.tokenUser = userId
                                           , DB.tokenCreated = now
                                           , DB.tokenExpires = Nothing
                                           , DB.tokenLastUse = Nothing
                                           , DB.tokenDeactivated = Nothing
                                           }
        let tokenId = P.unSqlBackendKey $ DB.unTokenKey key
        instances' <- getUserInstances userId
        return ( tokenId
               , ReturnLogin{ returnLoginToken = token'
                            , returnLoginInstances = instances'
                            }
               )
    createOTP twilioConf p userId = do
        otp' <- mkRandomOTP
        now <- liftIO getCurrentTime
        key <- runDB $ insert DB.UserOtp { DB.userOtpUser = userId
                                         , DB.userOtpPassword = otp'
                                         , DB.userOtpCreated = now
                                         , DB.userOtpDeactivated = Nothing
                                         }
        sendOTP twilioConf userEmail p otp'
        Log.logES Log.OTPSent{ Log.user = userEmail
                             , Log.otp = P.unSqlBackendKey $
                                          DB.unUserOtpKey key
                             }
        return ()

changePassword :: B64Token
               -> ChangePassword
               -> API (Either ChangePasswordError ())
changePassword tok ChangePassword { changePasswordOldPasword = oldPwd
                                  , changePasswordNewPassword = newPwd
                                  } = runExceptT $ do
  mbUser <- lift $ getUserByToken tok
  (_tokenID, usr) <- case mbUser of
    Nothing -> throwError ChangePasswordTokenError
    Just usr -> return usr
  mbError <- lift $ checkUserPassword (DB.userEmail usr) oldPwd
  case mbError of
    Left e -> do
      Log.logES Log.PasswordChangeFailed{ Log.user = usr ^. email}
      throwError $ ChangePasswordLoginError e
    Right _ -> Log.logES Log.PasswordChanged{ Log.user = usr ^. email}
  mbError' <- lift $ changeUserPassword (usr ^. DB.uuid) newPwd
  case mbError' of
    Nothing -> do
      Log.logES Log.PasswordChangeFailed{ Log.user = usr ^. email}
      throwError ChangePasswordHashError
    Just _ -> return ()


getUserByToken :: B64Token -> API (Maybe (Log.TokenRef, DB.User))
getUserByToken tokenId = do
  -- Delete expired tokens
  now <- liftIO $ getCurrentTime
  deactivateTokenWhere [DB.TokenExpires P.<=. Just now]

  user' <- runDB . select . E.from $ \(user' `InnerJoin` token') -> do
    on (user' E.^. DB.UserUuid ==. token' E.^. DB.TokenUser)
    whereL [ token' E.^. DB.TokenToken ==. val tokenId
           , E.isNothing $ token' E.^. DB.TokenDeactivated
           ]
    return (token' E.^. DB.TokenId, user')
  runDB $ P.updateWhere [DB.TokenToken P.==. tokenId]
                        [DB.TokenLastUse P.=. Just now]
  return . fmap (unTokenKey . unValue *** entityVal) $ listToMaybe user'

getUserInfo :: B64Token -> API (Maybe ReturnUserInfo)
getUserInfo token' = do
  mbUser <- getUserByToken token'
  Traversable.forM mbUser $ \(_tid, user') -> do
    instances' <- getUserInstances (user' ^. DB.uuid)
    return ReturnUserInfo { returnUserInfoId = user' ^. DB.uuid
                          , returnUserInfoEmail = user' ^. email
                          , returnUserInfoName = user' ^. name
                          , returnUserInfoPhone = user' ^. phone
                          , returnUserInfoInstances = instances'
                          }

checkTokenInstance :: Text -> B64Token -> InstanceID -> API (Maybe UserID)
checkTokenInstance request (B64Token "") inst = do
    Log.logES Log.RequestNoToken{ Log.request = request
                                , Log.instanceId = inst
                                }
    return Nothing
checkTokenInstance request tok inst = do
    mbUsr <- getUserByToken tok
    case mbUsr of
      Nothing -> do
        Log.logES Log.RequestInvalidToken{ Log.request = request
                                         , Log.token = unB64Token tok
                                         , Log.instanceId = inst
                                         }
        return Nothing
      Just (tid,  usr) -> do
        mbInst <- checkInstance inst (DB.userUuid usr)
        case mbInst of
          Nothing -> do
            Log.logES Log.RequestInvalidInstance{ Log.user = usr ^. email
                                                , Log.request = request
                                                , Log.tokenId = tid
                                                , Log.instanceId = inst
                                                }
            return Nothing
          Just _ -> return $ Just (DB.userUuid usr)

checkToken :: B64Token -> API (Maybe UserID)
checkToken tokenId = fmap (DB.userUuid . snd) <$> getUserByToken tokenId

checkInstance :: InstanceID -> UserID -> API (Maybe DB.UserInstance)
checkInstance inst user' = runDB $ P.get (DB.UserInstanceKey user' inst)

-- checkInstance :: InstanceID ->

logOut :: B64Token -> API ()
logOut token' = do
    mbUser <- getUserByToken token'
    case mbUser of
      Just (tid , usr) -> Log.logES Log.Logout{ Log.user = usr ^. email
                                              , Log.tokenId = tid
                                              }
      _ -> return ()
    deactivateTokenWhere [DB.TokenToken P.==. token']


-- addUser name password email mbPhone = do

closeOtherSessions :: B64Token -> API ()
closeOtherSessions tokenID = do
  runDB $ E.delete . E.from $ \tok -> do
    whereL [ E.exists . E.from $ \token' ->
              whereL [ tok E.^. DB.TokenUser E.==. token' E.^. DB.TokenUser
                     , token' E.^. DB.TokenToken E.==. E.val tokenID
                     ]
           , tok E.^. DB.TokenToken E.!=. E.val tokenID
           ]

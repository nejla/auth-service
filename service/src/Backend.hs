{-# LANGUAGE LambdaCase #-}
-- Copyright (c) 2015 Lambdatrade AB
-- All rights reserved

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Backend
  (module Backend
  ) where

import           Control.Arrow        ((***))
import           Control.Lens         hiding (from)
import           Control.Monad
import qualified Control.Monad.Catch  as Ex
import           Control.Monad.Except
import qualified Crypto.BCrypt        as BCrypt
import           Data.Maybe           (listToMaybe)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time.Clock
import qualified Data.Traversable     as Traversable
import qualified Data.UUID.V4         as UUID
import qualified Database.Esqueleto   as E
import           Database.Esqueleto   hiding ((^.), from)
import qualified Database.Persist     as P
import qualified Database.Persist.Sql as P
import           System.Random

import           NejlaCommon

import qualified Logging              as Log
import qualified Persist.Schema       as DB
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
       Log.logES Log.UserCreated{ Log.user = usr ^. email}
       _ <- runDB . P.insertMany $ for (usr ^. instances) $ \iid ->
           DB.UserInstance { DB.userInstanceUser = uid
                           , DB.userInstanceInstanceId = iid
                           }
       return $ Just uid

getUserByEmail :: Email -> API (Maybe DB.User)
getUserByEmail name' =
  fmap (fmap entityVal) . runDB $ P.getBy (DB.UniqueUserEmail name')

createResetToken :: NominalDiffTime -> UserID -> API PwResetToken
createResetToken expires usr = do
  tok <-
    unprivileged $ mkUniqueRandomHrID (Prelude.id) 20 DB.PasswordResetTokenToken
  now <- liftIO getCurrentTime
  _ <-
    db' $
    P.insert
      DB.PasswordResetToken
      { DB.passwordResetTokenToken = tok
      , DB.passwordResetTokenUser = usr
      , DB.passwordResetTokenCreated = now
      , DB.passwordResetTokenExpires = addUTCTime expires now
      , DB.passwordResetTokenUsed = Nothing
      }
  return tok

resetTokenActive ::
     UTCTime -> SqlExpr (Entity DB.PasswordResetToken) -> SqlExpr (Value Bool)
resetTokenActive now token =
  andL [ token E.^. DB.PasswordResetTokenExpires >. val now
       , E.isNothing $ token E.^. DB.PasswordResetTokenUsed
       ]


getUserByResetPwToken :: PwResetToken -> API DB.User
getUserByResetPwToken token = do
  now <- liftIO getCurrentTime
  users <- db' . E.select . E.from $ \((tok :: SV DB.PasswordResetToken)
                                       `InnerJoin` (usr :: SV DB.User)
                                      ) -> do
    onForeignKey tok usr
    whereL [ tok E.^. DB.PasswordResetTokenToken E.==. val token
           , resetTokenActive now tok
           ]
    return usr
  case users of
    [] -> do
      Log.logInfo $ "Failed reset password attempt: " <> token
      Ex.throwM ChangePasswordTokenError
    (user : _) -> return $ entityVal user


-- | Reset password using the token from a password reset email
resetPassword :: PwResetToken
              -> Password
              -> Maybe Password
              -> API (Either ChangePasswordError ())
resetPassword token password mbOtp = runExceptT $ do
  now <- liftIO getCurrentTime
  usr <- lift $ getUserByResetPwToken token
  let uid = DB.userUuid usr
  lift (handleOTP usr mbOtp) >>= \case
    Left e -> throwError $ ChangePasswordLoginError e
    Right _ -> return ()
 -- Set token to "used", return number of tokens this updated (0 or 1)
  cnt <- lift . db'. updateCount $ \(tok :: SV DB.PasswordResetToken) -> do
    E.set tok [DB.PasswordResetTokenUsed E.=. val (Just now)]
    whereL [ tok E.^. DB.PasswordResetTokenToken E.==. val token
           , tok E.^. DB.PasswordResetTokenUser E.==. val uid
           , resetTokenActive now tok
           ]
  -- If there was a valid token, cnt should be 1
  if cnt > 0
    then lift $ changeUserPassword uid password
    else do
    Log.logInfo $ "Failed reset password attempt (token not found): " <> token
    throwError ChangePasswordTokenError



changeUserPassword :: UserID -> Password -> API ()
changeUserPassword user' password' = do
  mbHash <- hashPassword password'
  cnt <-
    case mbHash of
      Nothing -> Ex.throwM ChangePasswordHashError
      Just hash ->
        runDB $
        P.updateWhereCount
          [DB.UserUuid P.==. user']
          [DB.UserPasswordHash P.=. hash]
  if cnt > 0
    then return ()
    else Ex.throwM ChangePasswordUserDoesNotExistError

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


sendOTP :: OtpHandler -> Email -> Phone -> Password -> API ()
sendOTP otpHandler (Email user') p (Password otp') = do
    Log.logInfo $ mconcat [ "Sending OTP for user " , user'
                          , "(", unPhone p , ")"
                          , ": " <> otp'
                          ]
    otpHandler p otp'

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
      if checkPassword hash pwd
        then do
          unless (hashUsesPolicy hash) $ rehash userId
          return $ Right usr
        else return $ Left LoginErrorFailed
  where
    checkPassword (PasswordHash hash) (Password pwd') =
      BCrypt.validatePassword hash (Text.encodeUtf8 pwd')
    hashUsesPolicy (PasswordHash hash) = BCrypt.hashUsesPolicy policy hash
    rehash userId = do
      mbNewHash <- hashPassword pwd
      case mbNewHash of
        Nothing -> return () -- TODO: log error
        Just newHash ->
          runDB $
          P.update (DB.UserKey userId) [DB.UserPasswordHash P.=. newHash]

deactivateOtpWhere :: [P.Filter DB.UserOtp] -> API ()
deactivateOtpWhere selector = do
  now <- liftIO getCurrentTime
  runDB $ P.updateWhere selector [DB.UserOtpDeactivated P.=. Just now]

deactivateTokenWhere :: [P.Filter DB.Token] -> API ()
deactivateTokenWhere selector = do
  now <- liftIO getCurrentTime
  runDB $ P.updateWhere selector [DB.TokenDeactivated P.=. Just now]

-- | Create and send one time password for a user
createOTP :: OtpHandler -> Phone -> Email -> UserID -> API ()
createOTP otpHandler p userEmail userId = do
  otp' <- mkRandomOTP
  now <- liftIO getCurrentTime
  key <-
    runDB $
    insert
      DB.UserOtp
      { DB.userOtpUser = userId
      , DB.userOtpPassword = otp'
      , DB.userOtpCreated = now
      , DB.userOtpDeactivated = Nothing
      }
  sendOTP otpHandler userEmail p otp'
  Log.logES
    Log.OTPSent
    { Log.user = userEmail
    , Log.otp = P.unSqlBackendKey $ DB.unUserOtpKey key
    }
  return ()

-- | Check that one time password is valid for user
checkOTP :: UserID -> Password -> API (Either LoginError (Entity DB.UserOtp))
checkOTP userId (Password otpC) = do
  let otp' = Password $ Text.toUpper otpC
  otpTime <- fromIntegral . negate <$> getConfig oTPTimeoutSeconds
  now <- liftIO getCurrentTime
  let cutoff = otpTime `addUTCTime` now
  deactivateOtpWhere [DB.UserOtpCreated P.<=. cutoff]
  checkOTP <-
    runDB $
    P.selectList
      [ DB.UserOtpUser P.==. userId
      , DB.UserOtpPassword P.==. otp'
      , DB.UserOtpDeactivated P.==. Nothing
      ]
      []
  case checkOTP of
    (k:_) -> do
      deactivateOtpWhere
        [DB.UserOtpUser P.==. userId, DB.UserOtpPassword P.==. otp']
      return $ Right k
    [] -> return $ Left LoginErrorFailed

handleOTP ::
     DB.User
  -> Maybe Password -- ^ Provided One Time Password (if any)
  -> API (Either LoginError (Maybe (Entity DB.UserOtp)))
handleOTP usr mbOtp = do
  let userId = usr ^. DB.uuid
  case mbOtp of
    Nothing -> do
      mbTwilioConf <- getConfig otp
      case mbTwilioConf of
        Nothing -> return $ Right Nothing
        Just twilioConf ->
          case DB.userPhone usr
                -- No phone number for two-factor auth
                of
            Nothing -> return $ Right Nothing
            Just p -> do
              createOTP twilioConf p (usr ^. email) userId
              return $ Left LoginErrorOTPRequired
    Just otpC ->
      checkOTP userId otpC >>= \case
        Left e -> do
          Log.logES $
            Log.AuthFailed
            { Log.user = usr ^. email
            , Log.reason = Log.AuthFailedReasonWrongOtp
            }
          return $ Left e
        Right k -> do
          return . Right $ Just k


login :: Login -> API (Either LoginError ReturnLogin)
login Login {loginUser = userEmail, loginPassword = pwd, loginOtp = mbOtp} = do
  mbError <- checkUserPassword userEmail pwd
  case mbError of
    Left e -> do
      Log.logES $
        Log.AuthFailed
        {Log.user = userEmail, Log.reason = Log.AuthFailedReasonWrongPassword}
      return $ Left e
    Right usr -> do
      handleOTP usr mbOtp >>= \case
        Left e -> return $ Left e
        Right _r -> do
          (tid, rl) <- createToken (usr ^. DB.uuid)
          Log.logES Log.AuthSuccess {Log.user = userEmail, Log.tokenId = tid}
          return $ Right rl
  where
    createToken userId = do
      now <- liftIO $ getCurrentTime
        -- token <- liftIO $ b64Token <$> getEntropy 16 -- 128 bits
      token' <- B64Token <$> mkRandomString tokenChars 22 -- > 128 bit
      key <-
        runDB . P.insert $
        DB.Token
        { DB.tokenToken = token'
        , DB.tokenUser = userId
        , DB.tokenCreated = now
        , DB.tokenExpires = Nothing
        , DB.tokenLastUse = Nothing
        , DB.tokenDeactivated = Nothing
        }
      let tokenId = P.unSqlBackendKey $ DB.unTokenKey key
      instances' <- getUserInstances userId
      return
        ( tokenId
        , ReturnLogin
          {returnLoginToken = token', returnLoginInstances = instances'})

changePassword :: B64Token
               -> ChangePassword
               -> API (Either ChangePasswordError ())
changePassword tok ChangePassword { changePasswordOldPasword = oldPwd
                                  , changePasswordNewPassword = newPwd
                                  , changePasswordOtp = otp
                                  } = runExceptT $ do
  mbUser <- lift $ getUserByToken tok
  (_tokenID, usr) <- case mbUser of
    Nothing -> do

      Log.logInfo $ "Failure while trying to change passowrd " <> (unB64Token tok)
      throwError ChangePasswordTokenError
    Just usr -> return usr
  mbError <- lift $ checkUserPassword (DB.userEmail usr) oldPwd
  case mbError of
    Left e -> do
      Log.logES Log.PasswordChangeFailed{ Log.user = usr ^. email}
      throwError $ ChangePasswordLoginError e
    Right _ -> Log.logES Log.PasswordChanged{ Log.user = usr ^. email}
  lift (handleOTP usr otp) >>= \case
    Left e -> throwError $ ChangePasswordLoginError e
    Right _ -> return ()

  mbError' <- Ex.try . lift $ changeUserPassword (usr ^. DB.uuid) newPwd

  case mbError' of
    Left e -> do
      Log.logES Log.PasswordChangeFailed{ Log.user = usr ^. email}
      throwError e
    Right{} -> return ()


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

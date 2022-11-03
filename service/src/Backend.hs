{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Backend
  ( module Backend
  ) where

import           Control.Arrow                        ((***))
import           Control.Lens                         hiding (from)
import           Control.Monad
import qualified Control.Monad.Catch                  as Ex
import           Control.Monad.Except
import qualified Crypto.BCrypt                        as BCrypt
import           Data.ByteString.Base64               as B64
import           Data.Maybe                           (listToMaybe, catMaybes)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Data.Time.Clock
import qualified Data.Traversable                     as Traversable
import qualified Data.UUID                            as UUID
import qualified Data.UUID.V4                         as UUID
import qualified Database.Esqueleto                   as E
import           Database.Esqueleto                   hiding ((^.), (<&>), from)
import           Database.Esqueleto.PostgreSQL        ( arrayAgg, arrayRemoveNull
                                                      , maybeArray)
import qualified Database.Persist                     as P
import qualified Database.Persist.Sql                 as P
import           System.Random

import           NejlaCommon                          as NC

import qualified Logging                              as Log
import qualified Persist.Schema                       as DB
import           Types
import           Database.Esqueleto.Internal.Internal (unsafeSqlBinOp)

import           Audit
import           Monad
import           Control.Monad.Logger                 (LoggingT(runLoggingT), askLoggerIO)

orLMb :: [Maybe (SqlExpr (Value Bool))]
       -> SqlExpr (Value Bool)
orLMb = orL . catMaybes

unOtpKey :: Key DB.UserOtp -> Log.OtpRef
unOtpKey = P.unSqlBackendKey . DB.unUserOtpKey

unTokenKey :: Key DB.Token -> Log.TokenRef
unTokenKey = P.unSqlBackendKey . DB.unTokenKey

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

policy :: BCrypt.HashingPolicy
policy = BCrypt.fastBcryptHashingPolicy { BCrypt.preferredHashCost = 10 }

hashPassword :: Password -> API (Maybe PasswordHash)
hashPassword (Password pwd) = liftIO $
    fmap PasswordHash <$>
      BCrypt.hashPasswordUsingPolicy policy (Text.encodeUtf8 pwd)

base64HashedPassword :: PasswordHash -> Text
base64HashedPassword (PasswordHash pwd) =
  "base64:" <> Text.decodeUtf8 (B64.encode pwd)

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
                            , DB.userDeactivate = Nothing
                            }
       _ <- runDB $ P.insert dbUser
       Log.logES Log.UserCreated{ Log.user = usr ^. email}
       _ <- runDB . P.insertMany $ for (usr ^. instances) $ \iid ->
           DB.UserInstance { DB.userInstanceUser = uid
                           , DB.userInstanceInstanceId = iid
                           }
       _ <- runDB. P.insertMany $ for (usr ^. roles) $ \role ->
           DB.UserRole{ DB.userRoleUser = uid
                      , DB.userRoleRole = role
                      }
       audit AuditUserCreated
           { auditUserID = uid
           , auditUserInstances = usr ^. instances
           , auditUserRoles = usr ^. roles
           }
       return $ Just uid

userAddRole :: UserID -> Text -> API ()
userAddRole usr role = do
  _ <- runDB $ P.insert DB.UserRole { DB.userRoleUser = usr
                                    , DB.userRoleRole = role
                                    }
  audit AuditUserRoleAdded
    { auditUserID = usr
    , auditUserRole = role
    }
  return ()

userRemoveRole :: UserID -> Text -> API ()
userRemoveRole usr role = do
  count <- runDB . E.deleteCount . E.from $ \(uRole :: SV DB.UserRole) -> do
    whereL [ uRole E.^. DB.UserRoleUser ==. val usr
           , uRole E.^. DB.UserRoleRole ==. val role
           ]
  case count of
    0 -> notFound "User Role" (usr, role)
    _ ->
      audit AuditUserRoleRemoved
            { auditUserID = usr
            , auditUserRole = role
            }


isDistinctFrom :: SqlExpr (Value (Maybe a)) -> SqlExpr (Value (Maybe a)) -> SqlExpr (Value Bool)
isDistinctFrom = unsafeSqlBinOp " IS DISTINCT FROM "

getUserByEmail :: Email -> API (Maybe DB.User)
getUserByEmail email' = do
  users <- runDB . E.select . E.from $ \(user :: SV DB.User) -> do
    where_ (lower_ (val email') ==. lower_ (user E.^. DB.UserEmail))
    -- limit 1, but LOWER ("email") has a UNIQUE INDEX
    return user
  return $ entityVal <$> listToMaybe users

createResetToken :: NominalDiffTime -> UserID -> API PwResetToken
createResetToken expiresIn usr = do
  tok <-
    unprivileged $ mkUniqueRandomHrID Prelude.id 20 DB.PasswordResetTokenToken
  now <- liftIO getCurrentTime
  let expires = addUTCTime expiresIn now
  _ <-
    db' $
    P.insert
      DB.PasswordResetToken
      { DB.passwordResetTokenToken = tok
      , DB.passwordResetTokenUser = usr
      , DB.passwordResetTokenCreated = now
      , DB.passwordResetTokenExpires = expires
      , DB.passwordResetTokenUsed = Nothing
      }
  audit AuditResetTokenCreated
        { auditUserID = usr
        , auditToken = tok
        , auditExpires = expires
        }
  return tok

resetTokenActive ::
     UTCTime -> SqlExpr (Entity DB.PasswordResetToken) -> SqlExpr (Value Bool)
resetTokenActive now token =
  andL [ token E.^. DB.PasswordResetTokenExpires >. val now
       , E.isNothing $ token E.^. DB.PasswordResetTokenUsed
       ]


getUserByResetPwToken :: PwResetToken -> API (Maybe DB.User)
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
      return Nothing
    (user : _) -> return . Just $ entityVal user


-- | Reset password using the token from a password reset email
resetPassword :: PwResetToken
              -> Password
              -> Maybe Password
              -> API (Either ChangePasswordError ())
resetPassword token password mbOtp = runExceptT $ do
  now <- liftIO getCurrentTime
  mbUser <- lift $ getUserByResetPwToken token
  usr <- case mbUser of
    Nothing -> throwError ChangePasswordTokenError
    Just usr -> return usr
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
    then do
    -- changeUserPassword will add to the audit log
      lift $ changeUserPassword (Just token) uid password
    else do
    Log.logInfo $ "Failed reset password attempt (token not found): " <> token
    throwError ChangePasswordTokenError



changeUserPassword :: Maybe PwResetToken -> UserID -> Password -> API ()
changeUserPassword mbToken user' password' = do
  mbHash <- hashPassword password'
  cnt <-
    case mbHash of
      Nothing -> Ex.throwM ChangePasswordHashError
      Just hash -> do
        cnt <- runDB $ P.updateWhereCount
                 [DB.UserUuid P.==. user']
                 [DB.UserPasswordHash P.=. hash]
        audit AuditUserPasswordChanged
              { auditUserID = user'
              , auditResetToken = mbToken
              , auditNewPasswordHash = base64HashedPassword hash
              }
        return cnt
  if cnt > 0
    then return ()
    else Ex.throwM ChangePasswordUserDoesNotExistError

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

haveInstance :: InstanceID -> API (Maybe DB.Instance)
haveInstance iid = do
  insts <- db' . E.select . E.from $ \(inst :: SV DB.Instance) -> do
    where_ (inst E.^. DB.InstanceUuid E.==. E.val iid)
    return inst
  return $ entityVal <$> listToMaybe insts

addInstance :: Maybe InstanceID -> Text -> API InstanceID
addInstance mbIid name' = do
  instanceID <- case mbIid of
                  Nothing -> liftIO $ InstanceID <$> UUID.nextRandom
                  Just iid -> return iid
  _ <- runDB . P.insert $ DB.Instance{ DB.instanceUuid = instanceID
                                     , DB.instanceName = name'
                                     }
  audit $ AuditInstanceAdded
          { auditInstanceID = instanceID
          , auditInstanceName = name'
          }
  return instanceID

addUserInstance :: UserID -> InstanceID -> API ()
addUserInstance user' inst = do
  _ <- runDB $ P.insert DB.UserInstance{ DB.userInstanceUser = user'
                                       , DB.userInstanceInstanceId = inst
                                       }
  audit AuditUserInstanceAdded
        { auditUserID = user'
        , auditInstanceID = inst
        }
  return ()

removeUserInstance :: UserID -> InstanceID -> API Integer
removeUserInstance user' inst = do
  count' <- runDB $ P.deleteWhereCount [ DB.UserInstanceUser P.==. user'
                                       , DB.UserInstanceInstanceId P.==. inst
                                       ]
  audit AuditUserInstanceRemoved
        { auditUserID = user'
        , auditInstanceID = inst
        }

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
    lfn <- askLoggerIO
    liftIO $ runLoggingT (otpHandler p otp') lfn

tokenChars :: [Char]
tokenChars = concat [ ['a' .. 'z']
                    , ['A' .. 'Z']
                    , ['0' .. '9']
                    ] -- Roughly 6 bit per char

checkUserPassword :: Email -> Password -> API (Either LoginError DB.User)
checkUserPassword userEmail pwd = do
  mbUser <- getUserByEmail userEmail
  now <- liftIO getCurrentTime
  case mbUser of
    Just usr | Just deac <- usr ^. deactivate
             , deac <= now
               -> return $ Left LoginErrorFailed
             | otherwise -> do
      let hash = usr ^. DB.passwordHash
          userId = usr ^. DB.uuid
      if checkPassword hash pwd
        then do
          unless (hashUsesPolicy hash) $ rehash userId
          return $ Right usr
        else return $ Left LoginErrorFailed
    Nothing -> return $ Left LoginErrorFailed
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

deactivateTokenWhere ::
     (SV DB.Token -> SqlExpr (Value Bool))
  -> API Int
deactivateTokenWhere selector = do
  now <- liftIO getCurrentTime
  fmap fromIntegral $ runDB $ updateCount $ \(tok :: SV DB.Token) -> do
    E.set tok [DB.TokenDeactivated E.=. val (Just now)]
    whereL [ selector tok
           -- This is necessary to give accurate counts
           , isNothing $ tok E.^. DB.TokenDeactivated
           ]

deactivateSsoTokenWhere ::
     (SV DB.SsoToken -> SqlExpr (Value Bool))
  -> API Int
deactivateSsoTokenWhere selector = do
  now <- liftIO getCurrentTime
  fmap fromIntegral $ runDB $ updateCount $ \(tok :: SV DB.SsoToken) -> do
    E.set tok [DB.SsoTokenDeactivated E.=. val (Just now)]
    whereL [ selector tok
           -- This is necessary to give accurate counts
           , isNothing $ tok E.^. DB.SsoTokenDeactivated
           ]

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
  audit AuditOTPCreated
        { auditUserID = userId
        , auditOTP = unPassword otp'
        , auditPhone = unPhone p
        }
  return ()

-- | Check that one time password is valid for user and deactivate it
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

checkLoginRateLimit ::
     UTCTime
  -> Int
  -> Text
  -> Email
  -> API (Either LoginError ())
checkLoginRateLimit cutoff max address (Email email) = do
  -- First clean up table
  db' $ E.delete . E.from $ \(attempt :: SV DB.LoginAttempt) -> do
    whereL  [ attempt E.^. DB.LoginAttemptTime E.<. E.val cutoff ]
  [E.Value attempts] <-
    db' $ E.select . E.from $ \(attempt :: SV DB.LoginAttempt) -> do
      whereL [ attempt E.^. DB.LoginAttemptTime >=. val cutoff
             , attempt E.^. DB.LoginAttemptEmail ==. val email
             , attempt E.^. DB.LoginAttemptRemoteAddress ==. val address
             ]
      return countRows
  return $ if attempts >= max
    then Left LoginErrorRatelimit
    else Right ()

rateLimitAddAttempt ::
     UTCTime
  -> Text
  -> Email
  -> API ()
rateLimitAddAttempt now address (Email email) = do
    _ <- db' $ insert DB.LoginAttempt
      { DB.loginAttemptTime = now
      , DB.loginAttemptRemoteAddress = address
      , DB.loginAttemptEmail = email
      }
    return ()

login ::
     NominalDiffTime
  -> Text
  -> Int
  -> Login
  -> API (Either LoginError ReturnLogin)
login timeframe remoteAddress maxAttempts
      Login{loginUser = userEmail, loginPassword = pwd, loginOtp = mbOtp}
    = runExceptT $ do
        now <- liftIO getCurrentTime
        let cutoff = addUTCTime (negate timeframe) now
        checkLoginRateLimit cutoff maxAttempts remoteAddress userEmail
                 `logFailed` Log.AuthFailedReasonRateLimit
        usr <- (checkUserPassword userEmail pwd >>= \case
                 x@Left{} -> do
                   rateLimitAddAttempt now remoteAddress userEmail
                   return x
                 x -> return x)
                   `logFailed` Log.AuthFailedReasonWrongPassword
        _ <- handleOTP usr mbOtp
                 `logFailed` Log.AuthFailedReasonWrongOtp

        (tid, rl) <- createToken (usr ^. DB.uuid)
        Log.logES Log.AuthSuccess {Log.user = userEmail, Log.tokenId = tid}
        return rl
  where
    logFailed :: API (Either LoginError b)
              -> Log.AuthFailedReason
              -> ExceptT LoginError (App ApiState 'Privileged 'ReadCommitted) b
    logFailed m reason = do
      lift m >>= \case
        Left e -> do
            Log.logES $ Log.AuthFailed
              { Log.user = userEmail
              , Log.reason = reason
              }
            throwError e
        Right r -> return r
    createToken userId = lift $ do
      now <- liftIO getCurrentTime
      -- We set the absolute expiration time for the token here, but the
      -- expiration timeout for tokens that haven't been used in a while is
      -- re-calculated on each check
      mbTokenExpiration <- getConfig timeout
      let tokenExpires = mbTokenExpiration <&> \texp ->
            -- fromInteger on NominalDiffTime assumes seconds
            fromInteger texp `addUTCTime` now
      token' <- B64Token <$> mkRandomString tokenChars 22 -- > 128 bit
      key <-
        runDB . P.insert $
        DB.Token
        { DB.tokenToken = token'
        , DB.tokenUser = userId
        , DB.tokenCreated = now
        , DB.tokenExpires = tokenExpires
        , DB.tokenLastUse = Nothing
        , DB.tokenDeactivated = Nothing
        }
      let tokenId = P.unSqlBackendKey $ DB.unTokenKey key
      instances' <- getUserInstances userId
      audit AuditTokenCreated
            { auditUserID = userId
            , auditOTPUsed = unPassword <$> mbOtp
            , auditCreatedToken = unB64Token token'
            }
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

      Log.logInfo $ "Failure while trying to change password " <> unB64Token tok
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

  -- changeUserPassword adds to the audit log
  mbError' <- Ex.try . lift $ changeUserPassword Nothing (usr ^. DB.uuid) newPwd

  case mbError' of
    Left e -> do
      Log.logES Log.PasswordChangeFailed{ Log.user = usr ^. email}
      throwError e
    Right{} -> return ()

getUserByToken :: B64Token -> API (Maybe (Log.TokenRef, DB.User))
getUserByToken tokenId = do
  -- Delete expired tokens
  now <- liftIO getCurrentTime
  mbTokenUnusedExpiration <- getConfig tokenUnusedTimeout
  let mbExpiresUnused = mbTokenUnusedExpiration <&> \tuexp ->
        addUTCTime (negate $ fromInteger tuexp) now
  deactivateTokenWhere $ \tok ->
    orLMb
    [ Just (tok E.^. DB.TokenExpires E.<=. val (Just now))
    -- Expire token when it was last used before `now - $TOKEN_UNUSED_TIMEOUT`
    , mbExpiresUnused <&> \expiresUnused ->
        -- If the token was never used, treat the creation as the first use
        coalesceDefault [tok E.^. DB.TokenLastUse]
          (tok E.^. DB.TokenCreated)
        -- now - $TOKEN_UNUSED_TIMEOUT
        E.<=. val expiresUnused
    ]

  user' <- runDB . select . E.from $ \(user' `InnerJoin` token') -> do
    on (user' E.^. DB.UserUuid ==. token' E.^. DB.TokenUser)
    whereL [ token' E.^. DB.TokenToken ==. val tokenId
           , E.isNothing $ token' E.^. DB.TokenDeactivated
           -- Check that user is not deactivated
           , orL [ isNothing $ user' E.^. DB.UserDeactivate
                 , user' E.^. DB.UserDeactivate >=. just (val now)
                 ]
           ]
    return (token' E.^. DB.TokenId, user')
  runDB $ P.updateWhere [DB.TokenToken P.==. tokenId]
                        [DB.TokenLastUse P.=. Just now]
  return . fmap (unTokenKey . unValue *** entityVal) $ listToMaybe user'

getSsoToken tokenId = do
  now <- liftIO getCurrentTime
  mbTokenUnusedExpiration <- getConfig tokenUnusedTimeout
  let mbExpiresUnused = mbTokenUnusedExpiration <&> \tuexp ->
        addUTCTime (negate $ fromInteger tuexp) now
  deactivateSsoTokenWhere $ \tok ->
    orLMb
    [ Just (tok E.^. DB.SsoTokenExpires E.<=. val (Just now))
    -- Expire token when it was last used before `now - $TOKEN_UNUSED_TIMEOUT`
    , mbExpiresUnused <&> \expiresUnused ->
        -- If the token was never used, treat the creation as the first use
        coalesceDefault [tok E.^. DB.SsoTokenLastUse]
          (tok E.^. DB.SsoTokenCreated)
        -- now - $TOKEN_UNUSED_TIMEOUT
        E.<=. val expiresUnused
    ]
  tok <- runDB . select . E.from $ \((ssoToken :: SV DB.SsoToken)
                                     `LeftOuterJoin` (ssoRole :: SVM DB.SsoTokenRole)
                                    ) -> do
    on (foreignKeyL ssoRole ssoToken)
    groupBy ( ssoToken E.^. DB.SsoTokenToken
            , ssoToken E.^. DB.SsoTokenUserId
            , ssoToken E.^. DB.SsoTokenEmail
            , ssoToken E.^. DB.SsoTokenName
            )
    whereL [ ssoToken E.^. DB.SsoTokenToken E.==. E.val tokenId
           , E.isNothing $ ssoToken E.^. DB.SsoTokenDeactivated
           ]
    return ( ssoToken E.^. DB.SsoTokenUserId
           , ssoToken E.^. DB.SsoTokenEmail
           , ssoToken E.^. DB.SsoTokenName
           , ssoToken E.^. DB.SsoTokenInstanceId
           , arrayRemoveNull
             $ maybeArray (arrayAgg (ssoRole E.?. DB.SsoTokenRoleRole))

           )
  runDB $ P.updateWhere [DB.SsoTokenToken P.==. tokenId]
                        [DB.SsoTokenLastUse P.=. Just now]
  return $ listToMaybe [ (uid, email, name, inst, roles) |
                         ( Value uid, Value email, Value name, Value inst
                         , Value roles
                         ) <- tok]

getUserRoles :: UserID -> API [Text]
getUserRoles uid =
  fmap (unValue <$>) . runDB . select . E.from $ \(uRole :: SV DB.UserRole) -> do
    whereL [ uRole E.^. DB.UserRoleUser ==. val uid]
    orderBy [asc $ uRole E.^. DB.UserRoleRole]
    return (uRole E.^. DB.UserRoleRole)

getUserInfo :: B64Token -> API (Maybe ReturnUserInfo)
getUserInfo token'
  | isSsoToken token' = do
      mbTok <- getSsoToken token'
      Traversable.forM mbTok $ \(uid, email, name, iid, roles) -> do
        mbInst <- getInstance iid
        return ReturnUserInfo { returnUserInfoId = uid
                            , returnUserInfoEmail = email
                            , returnUserInfoName = name
                            , returnUserInfoPhone = Nothing
                            , returnUserInfoInstances =
                                [ ReturnInstance
                                  { returnInstanceName =
                                      maybe "" DB.instanceName mbInst
                                  , returnInstanceId = iid
                                  }]
                            , returnUserInfoRoles = roles
                            , returnUserInfoDeactivate = Nothing
                            }
  | otherwise = do
  mbUser <- getUserByToken token'
  Traversable.forM mbUser $ \(_tid, user') -> do
    instances' <- getUserInstances (user' ^. DB.uuid)
    roles <- getUserRoles (user' ^. DB.uuid)
    return ReturnUserInfo { returnUserInfoId = user' ^. DB.uuid
                                               . to (UUID.toText . unUserID )
                          , returnUserInfoEmail = user' ^. email
                          , returnUserInfoName = user' ^. name
                          , returnUserInfoPhone = user' ^. phone
                          , returnUserInfoInstances = instances'
                          , returnUserInfoRoles = roles
                          , returnUserInfoDeactivate = user' ^. deactivate
                          }

isSsoToken :: B64Token -> Bool
isSsoToken (B64Token tok) = "sso:" `Text.isPrefixOf` tok

checkTokenInstance :: Text
                   -> B64Token
                   -> InstanceID
                   -> API (Maybe (Text , Email, Name, [Text]
                                 ))
checkTokenInstance request (B64Token "") inst = do
    Log.logES Log.RequestNoToken{ Log.request = request
                                , Log.instanceId = inst
                                }
    return Nothing

checkTokenInstance request tok inst
  | isSsoToken tok = do
      mbToken <- getSsoToken tok
      case mbToken of
        Nothing -> do
          Log.logES  Log.RequestInvalidToken{ Log.request = request
                                            , Log.token = unB64Token tok
                                            , Log.instanceId = inst
                                            }
          return Nothing
        Just (uid, email, name, _inst,  roles) -> do
          return $ Just (uid, email, name, roles)

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
          Just _ -> do
            let uid = DB.userUuid usr
            roles <- getUserRoles uid
            return $ Just ( UUID.toText $ unUserID uid
                          , DB.userEmail usr
                          , DB.userName usr
                          , roles
                          )

data IsAdmin = IsAdmin -- Don't export Constructor
  deriving (Show, Eq)

-- | Check that the token represents an Admin
checkAdmin :: Text
           -> B64Token
           -> API (Maybe IsAdmin)
checkAdmin request tok = do
  getUserByToken tok >>= \case
    Nothing -> do
      Log.logES Log.AdminRequestInvalidToken{ Log.token = unB64Token tok
                                            , Log.request = request
                                            }

      return Nothing
    Just usr -> do
      roles' <- getUserRoles (usr ^. _2 . uuid)
      if "admin" `elem` roles'
        then return $ Just IsAdmin -- @TODO
        else do
          Log.logES Log.AdminRequestNotAdmin
                   { Log.request = request
                   , Log.token = unB64Token tok
                   }
          return Nothing

checkToken :: B64Token -> API (Maybe Text)
checkToken token
  | isSsoToken token = do
      fmap (\(uid, _email, _name, _inst, _roles) -> uid ) <$> getSsoToken token
checkToken token = fmap (UUID.toText . unUserID . DB.userUuid . snd)
                       <$> getUserByToken token
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
    _ <- deactivateTokenWhere (\tok -> tok E.^. DB.TokenToken E.==. val token')
    audit AuditTokenDeactivated { auditToken = unB64Token token'}
-- addUser name password email mbPhone = do

closeOtherSessions :: B64Token -> API ()
closeOtherSessions tokenID = do
  cnt <- deactivateTokenWhere $ \tok -> do
    andL [ E.exists . E.from $ \token' ->
            whereL [ tok E.^. DB.TokenUser E.==. token' E.^. DB.TokenUser
                   , token' E.^. DB.TokenToken E.==. E.val tokenID
                   ]
         , tok E.^. DB.TokenToken E.!=. E.val tokenID
         ]
  when (cnt > 0) $
    audit AuditOtherTokensDeactivated { auditToken = unB64Token tokenID }

--------------------------------------------------------------------------------
-- Admin endpoints -------------------------------------------------------------
--------------------------------------------------------------------------------

getUsersBy :: ( SV DB.User -> SqlExpr (Value Bool))
              -> API [ReturnUserInfo]
getUsersBy selector = do
  users <- runDB $ E.select . E.from $ \((user :: SV DB.User)
                                        `LeftOuterJoin` (role :: SVM DB.UserRole)
                                        `LeftOuterJoin`
                                          ((uinst :: SVM DB.UserInstance)
                                           `InnerJoin` (inst :: SVM DB.Instance)
                                          )

                                        ) -> do
    on $ foreignKeyLR uinst inst
    on $ foreignKeyL uinst user
    on $ foreignKeyL role user
    where_ $ selector user
    groupBy ( user E.^. DB.UserUuid
            , user E.^. DB.UserEmail
            , user E.^. DB.UserName
            , user E.^. DB.UserPhone
            , user E.^. DB.UserDeactivate
            )
    orderBy [E.asc $ user E.^. DB.UserEmail]
    return ( user E.^. DB.UserUuid
           , user E.^. DB.UserEmail
           , user E.^. DB.UserName
           , user E.^. DB.UserPhone
           , user E.^. DB.UserDeactivate
           , arrayAgg' $ role ?. DB.UserRoleRole
           , arrayAgg' $ inst ?. DB.InstanceUuid
           , arrayAgg' $ inst ?. DB.InstanceName
           )

  return $ for users $ \(Value userId, Value userEmail, Value userName
                        , Value userPhone
                        , Value userDeactivate
                        , Value roles
                        , Value instanceIDs , Value instanceNames) ->
    ReturnUserInfo
    { returnUserInfoId         = UUID.toText $ unUserID userId
    , returnUserInfoEmail      = userEmail
    , returnUserInfoName       = userName
    , returnUserInfoPhone      = userPhone
    , returnUserInfoInstances  = zipWith ReturnInstance instanceNames instanceIDs
    , returnUserInfoRoles      = roles
    , returnUserInfoDeactivate = userDeactivate
    }
 where
   for = flip fmap

getUsers :: API [ReturnUserInfo]
getUsers = getUsersBy (\ _ -> val True)

getUsersByUids :: [Text] -> API [ReturnUserInfo]
getUsersByUids uids =
  let uuids = [UserID uuid | Just uuid <- UUID.fromText <$> uids ]
  -- We should eventually change the userID type from UUID to Text to conform to
  -- the public API. However, for now, leaving the internal Type be a UUID is OK
  in getUsersBy ( \user -> (user E.^. DB.UserUuid) `E.in_` valList uuids
                )

getUsersByRole :: Text -> API [ReturnUserInfo]
getUsersByRole role = getUsersBy $
  \user -> exists . E.from $ \(userRole :: SV DB.UserRole) ->
     whereL [ foreignKey userRole user
            , userRole E.^. DB.UserRoleRole ==. E.val role
            ]

deactivateUser :: UserID -> DeactivateAt -> API ()
deactivateUser uid deactivate = do
  deactivateAt <- case deactivate of
                    DeactivateNow -> liftIO getCurrentTime
                    DeactivateAt time -> return time
  num <- db' $ P.updateWhereCount [ DB.UserUuid P.==. uid]
                                  [ DB.UserDeactivate P.=. Just deactivateAt ]
  case num of
    0 -> NC.notFound "user by uuid" uid
    1 -> audit AuditUserDeactivated
               { auditUserID = uid
               , auditDeactivateAt = deactivateAt
               }
    _ -> error $ "deactivateUser: More than one user affected: <> " ++ show num

reactivateUser :: UserID -> API ()
reactivateUser uid = do
  num <- db' $ P.updateWhereCount [ DB.UserUuid P.==. uid]
                                  [ DB.UserDeactivate P.=. Nothing ]
  case num of
    0 -> NC.notFound "user by uuid" uid
    1 -> audit AuditUserReactivated
               { auditUserID = uid
               }

    _ -> error $ "deactivateUser: More than one user affected: <> " ++ show num

deleteUser :: UserID -> API ()
deleteUser uid = do
  db' $ P.deleteWhere [DB.TokenUser P.==. uid]
  db' $ P.deleteWhere [DB.PasswordResetTokenUser P.==. uid]
  db' $ P.deleteWhere [DB.UserRoleUser P.==. uid]
  db' $ P.deleteWhere [DB.UserOtpUser P.==. uid]
  db' $ P.deleteWhere [DB.UserInstanceUser P.==. uid]
  cnt <- db' $ P.deleteWhereCount [DB.UserUuid P.==. uid]
  case cnt of
    0 -> NC.notFound "user by uuid" uid
    _ -> audit AuditUserDeleted{ auditUserID = uid }

storeAssertionID :: Text -> API ()
storeAssertionID id = do
  _ <- db' $ P.insert (DB.AssertionId id)
  return ()

getInstance :: InstanceID -> API (Maybe DB.Instance)
getInstance instId = do
  inst <- db' . E.select . E.from $ \(inst :: SV DB.Instance) -> do
    E.where_ (inst E.^. DB.InstanceUuid E.==. E.val instId)
    return inst
  return (entityVal <$> listToMaybe inst)

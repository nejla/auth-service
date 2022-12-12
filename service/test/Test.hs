{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

-- | API tests

module Main where

import           Control.Lens
import qualified Control.Monad.Catch     as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Data
import           Data.IORef
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.UUID               (UUID)
import qualified Data.UUID               as UUID
import qualified Data.UUID.V4            as UUID
import           Database.Persist.Sql    (ConnectionPool)
import qualified Prelude
import           Prelude                 hiding (id)
import           Test.Hspec.Expectations
import qualified Test.QuickCheck.Monadic as QC
import           Test.Tasty              (defaultMain, testGroup, TestTree)
import           Test.Tasty.HUnit        hiding (assertFailure)
import           Test.Tasty.QuickCheck
import qualified Text.Microstache        as Mustache

import           Backend
import           Monad
import           PasswordReset
import qualified Persist.Schema          as DB
import           SAML
import           Types

import           Test.Common

newtype AssertionFailed = AssertionFailed String deriving (Typeable)

instance Show AssertionFailed where
  show (AssertionFailed e) = "Assertion Failed: " <>  e

instance Ex.Exception AssertionFailed

assertFailure :: Ex.MonadThrow m => String -> m a
assertFailure = Ex.throwM  . AssertionFailed

type Case a = ConnectionPool -> IO a

loginDefaults :: Login -> API (Either LoginError ReturnLogin)
-- 100 attempts every 1 second, empty remote address
loginDefaults = login 1 "" 100

--------------------------------------------------------------------------------
-- Add User
--------------------------------------------------------------------------------

defaultInstance :: InstanceID
defaultInstance = InstanceID [uuidQ|3c8519a3-f1ab-4549-a9fd-d4329a6a193a|]

testUser :: AddUser
testUser = AddUser { addUserUuid = Nothing
                   , addUserEmail = Email "no@spam.please"
                   , addUserPassword = Password "pwd"
                   , addUserName = "Jon Doe"
                   , addUserPhone = Nothing
                   , addUserInstances = []
                   , addUserRoles = []
                   }


testUserOtp :: AddUser
testUserOtp = testUser & phone ?~ Phone "12345"

withUser :: AddUser -> (UserID -> (forall a. API a -> IO a) -> IO ()) -> Case ()
withUser usr f pool = withRunAPI Prelude.id pool $ \run -> do
  mbRes <- run $ createUser usr
  case mbRes of
    Nothing -> assertFailure "could not create user"
    Just uid -> f uid run

ssoTestUser :: AddUser
ssoTestUser = testUser & instances .~ [defaultInstance]

withSsoToken :: AddUser
             -> (B64Token -> Text -> (forall a. API a -> IO a) -> IO ())
             -> Case ()
withSsoToken usr f pool = withRunAPI Prelude.id pool $ \run -> do
  uid <- UUID.toText <$> maybe UUID.nextRandom (return . unUserID) (usr ^. uuid)
  res <- case usr ^. instances of
    [iid] -> do
      _ <- run $ addInstance (Just iid) "default instance"
      run $ createSsoToken uid (usr ^. email) (usr ^. name) iid "" (usr ^. roles)
    _ -> fail "No instance ID"
  f (res ^. token) uid run

withUserOTP ::
     AddUser
  -> (UserID -> IO (Maybe Text) -> (forall a. API a -> IO a) -> IO ())
  -> Case ()
withUserOTP usr f pool = do
  otpRef <- newIORef Nothing
  let otpHandler _ = liftIO . writeIORef otpRef . Just
      readOTP = do
        res <- readIORef otpRef
        writeIORef otpRef Nothing
        return res
  withRunAPI (otp ?~ otpHandler) pool $ \run -> do
    mbRes <- run $ createUser usr
    case mbRes of
      Nothing -> assertFailure "could not create user"
      Just uid -> f uid readOTP run


checkUser :: DB.User -> AddUser -> IO ()
checkUser dbUser addUser = do
  dbUser ^. email `shouldBe` addUser ^. email
  dbUser ^. name `shouldBe`  addUser ^. name
  dbUser ^. phone `shouldBe` addUser ^. phone

loginOTP ::
     (API (Either LoginError ReturnLogin) -> IO (Either LoginError ReturnLogin))
  -> IO (Maybe Text)
  -> AddUser
  -> IO B64Token
loginOTP run getOtp AddUser{..} = do
    res <- run $ loginDefaults Login { loginUser = addUserEmail
                             , loginPassword = addUserPassword
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otp <- getOtp
    res' <- run $ loginDefaults Login { loginUser = addUserEmail
                                      , loginPassword = addUserPassword
                                      , loginOtp = Just $ Password otp
                                      }
    case res' of
      Left e -> assertFailure $ "Failed login with OTP " <> show e
      Right r -> return $ returnLoginToken r

case_create_user :: Case ()
case_create_user = withUser testUser $ \_uid run -> do
  mbUsr <- run . getUserByEmail $ testUser ^. email
  case mbUsr of
    Nothing -> assertFailure "Did not get user"
    Just usr -> checkUser usr testUser

case_user_check_password :: Case ()
case_user_check_password = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> assertFailure "check password failed"
    Right _ -> return ()

case_user_check_password_wrong :: Case ()
case_user_check_password_wrong = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (Password "bogus")
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Accepted bogus password"

case_user_email_case_insensitive :: Case ()
case_user_email_case_insensitive = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword "NO@SpAM.pleaSE" (Password "pwd")
  case res of
    Left _e -> assertFailure "Did not accept case-altered email"
    Right{} -> return ()

--------------------------------------------------------------------------------
-- Password Changes
--------------------------------------------------------------------------------

case_user_change_password :: Case ()
case_user_change_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword Nothing uid (Password "newPassword")
  res <- run $ checkUserPassword (testUser ^. email) (Password "newPassword")
  case res of
    Left _e -> assertFailure "check password fails"
    Right _ -> return ()

case_user_change_password_old_password :: Case ()
case_user_change_password_old_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword Nothing uid (Password "newpassword")
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Could still use old password"

case_changePassword_otp :: Case ()
case_changePassword_otp =
  withUserOTP testUserOtp $ \uid getOtp run -> do
    tok <- loginOTP run getOtp testUserOtp
    res <-
      run $
      changePassword
        tok
        ChangePassword
        { changePasswordOldPassword = testUserOtp ^. password
        , changePasswordNewPassword = "pwd2"
        , changePasswordOtp = Nothing
        }
    res `shouldBe` Left (ChangePasswordLoginError LoginErrorOTPRequired)
    Just otp <- getOtp

    res' <-
      run $
      changePassword
        tok
        ChangePassword
        { changePasswordOldPassword = testUserOtp ^. password
        , changePasswordNewPassword = "pwd2"
        , changePasswordOtp = Just $ Password otp
        }
    res' `shouldBe` Right()
    return ()

--------------------------------------------------------------------------------
-- Reset Password
--------------------------------------------------------------------------------

case_reset_password :: Case ()
case_reset_password = withUser testUser $ \uid run -> do
  tok <- run $ createResetToken 60 uid
  _ <- run $ resetPassword tok "newPwd" Nothing
  _ <- run $ checkUserPassword (testUser ^. email) "newPwd"
  return ()

case_reset_password_wrong_token :: Case ()
case_reset_password_wrong_token pool =
  withRunAPI Prelude.id pool $ \run -> do
    run (resetPassword "BogusToken" "newPwd" Nothing) `shouldReturn`
      Left ChangePasswordTokenError
    return ()

case_reset_password_OTP :: Case ()
case_reset_password_OTP = withUserOTP testUserOtp $ \uid getOtp run -> do
  tok <- run $ createResetToken 60 uid
  res <- run (resetPassword tok "newPwd" Nothing)
  res `shouldBe` Left (ChangePasswordLoginError LoginErrorOTPRequired)
  Just otp <- getOtp
  run (resetPassword tok "newPwd" $ Just (Password otp))
  return ()

case_reset_password_double_use :: Case ()
case_reset_password_double_use =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken 60 uid
    run $ resetPassword tok "newPwd" Nothing
    run (resetPassword tok "newPwd2" Nothing) `shouldReturn`
      Left ChangePasswordTokenError
    return ()

case_reset_password_expired :: Case ()
case_reset_password_expired =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken (-60) uid
    run (resetPassword tok "newPwd" Nothing) `shouldReturn`
      Left ChangePasswordTokenError
    return ()

testEmailData :: EmailData
testEmailData =
  EmailData
  { emailDataToken = "abc"
  , emailDataExpirationTime = "24"
  }

case_password_reset_render_email :: Case ()
case_password_reset_render_email _pool = do
  renderedEmail <-
    runNoLoggingT $
    renderEmail
      testEmailConfig
      (testEmailConfig ^. pWResetTemplate)
      (Just "tok123abc")
  renderedEmail `shouldBe`
    "please click on http://localhost/reset?token=tok123abc"

case_password_reset_render_error_email :: Case ()
case_password_reset_render_error_email _pool = do
  renderedEmail <-
    runNoLoggingT $
    renderEmail
      testEmailConfig
      (testEmailConfig ^. pWResetUnknownTemplate)
      Nothing
  renderedEmail `shouldBe`
    "Your email is unknown"

-- | Check that the render functions throws an exception when there are errors
-- during render
case_password_reset_render_email_errors :: Case ()
case_password_reset_render_email_errors _pool = do
  let tmpl = case Mustache.compileMustacheText "test" "{{bogus}}" of
               Right t -> t
               Left e -> error $ show e
  runNoLoggingT (renderEmail testEmailConfig tmpl Nothing)
    `shouldThrow` (== EmailRenderError)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

case_add_user_instance :: Case ()
case_add_user_instance = withUser testUser $ \uid run -> do
  iid <- run $ addInstance Nothing "instance1"
  run $ addUserInstance uid iid
  iids <- run $ getUserInstances uid
  iids ^.. each . id  `shouldBe` [iid]

case_remove_user_instance :: Case ()
case_remove_user_instance = withUser testUser $ \uid run -> do
  iid <- run $ addInstance Nothing "instance1"
  run $ addUserInstance uid iid
  _ <- run $ removeUserInstance uid iid

  iids <- run $ getUserInstances uid
  iids `shouldBe` []

--------------------------------------------------------------------------------
-- MkRandomString
--------------------------------------------------------------------------------

prop_mkRandomString_length :: Int -> Property
prop_mkRandomString_length len = QC.monadicIO $ do
  let l = abs len
  str <- QC.run $ mkRandomString otpIDChars l
  QC.assert (Text.length str == l)

prop_mkRandomString_chars :: Int -> Int -> Property
prop_mkRandomString_chars len numChars = QC.monadicIO $ do
  let l = abs len
  let chars = take (max 1 numChars) otpIDChars
  str <- QC.run $ mkRandomString chars l
  QC.assert (Text.all (`elem` chars) str)

--------------------------------------------------------------------------------
-- Login
--------------------------------------------------------------------------------

runLogin :: (forall a . API a -> IO a)
         -> AddUser
         -> IO ReturnLogin
runLogin run usr = do
  res <- run $ loginDefaults Login{ loginUser = usr ^. email
                          , loginPassword = usr ^. password
                          , loginOtp = Nothing
                          }
  case res of
    Left _e -> assertFailure "Could not login"
    Right rl -> return rl

case_login :: Case ()
case_login = withUser testUser $ \_uid run -> do
  _ <- runLogin run testUser
  return ()

case_login_otp :: Case ()
case_login_otp =  withUserOTP testUserOtp $ \_uid getOtp run -> do
    res <- run $ loginDefaults Login { loginUser = testUserOtp ^. email
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otp <- getOtp
    res' <- run $ loginDefaults Login { loginUser = testUserOtp ^. email
                              , loginPassword = testUserOtp ^. password
                              , loginOtp = Just $ Password otp
                              }
    case res' of
      Left e -> assertFailure $ "Failed login with OTP " <> show e
      Right _ -> return ()

case_login_otp_wrong_user :: Case ()
case_login_otp_wrong_user =
  withUserOTP testUserOtp $ \_uid getOtp run -> do
    _ <- run . createUser $ testUserOtp & email .~ "user2@spam.please"

    -- First, log in alternative user and get OTP
    res <- run $ loginDefaults Login { loginUser = "user2@spam.please"
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otpUser2 <- getOtp
    -- Now log in test user, ignore OTP
    res <- run $ loginDefaults Login { loginUser = testUserOtp ^. email
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired


    -- Finally, try to log in test user with alternative user's OTP
    res' <- run $ loginDefaults Login { loginUser = testUserOtp ^. email
                              , loginPassword = testUserOtp ^. password
                              , loginOtp = Just $ Password otpUser2
                              }

    res' `shouldBe` Left LoginErrorFailed

withUserToken :: AddUser
              -> (B64Token -> UserID -> (forall a. API a -> IO a) -> IO ())
              -> Case ()
withUserToken usr f = withUser usr $ \uid run -> do
  res <- runLogin run usr
  f (res ^. token) uid run


case_checkToken :: Case ()
case_checkToken = withUserToken testUser $ \tok uid run -> do
  res <- run $ checkToken tok
  res `shouldBe` Just (UUID.toText $ unUserID uid)

case_checkToken_bogus :: Case ()
case_checkToken_bogus = withUser testUser $ \_uid run -> do
  let tok = B64Token "bogus"
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_checkToken_expired :: Case ()
case_checkToken_expired pool = withRunAPI (timeout ?~ 0) pool $ \run -> do
  _uid <- run $ createUser testUser
  res1 <- runLogin run testUser
  let tok = res1 ^. token
  res2 <- run $ checkToken tok
  res2 `shouldBe` Nothing

case_checkToken_unused_expired :: Case ()
case_checkToken_unused_expired pool =
  withRunAPI (tokenUnusedTimeout ?~ 0) pool $ \run -> do
    _uid <- run $ createUser testUser
    res1 <- runLogin run testUser
    let tok = res1 ^. token
    res2 <- run $ checkToken tok
    res2 `shouldBe` Nothing

case_checkTokenInstance :: Case ()
case_checkTokenInstance = withUserToken testUser $ \tok uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  run $ addUserInstance uid iid
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Just ((UUID.toText $ unUserID uid)
                      , addUserEmail testUser, addUserName testUser
                      , []
                      )

case_checkTokenInstance_not_member :: Case ()
case_checkTokenInstance_not_member = withUserToken testUser $ \tok _uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Nothing


case_checkSsoTokenInstance :: Case ()
case_checkSsoTokenInstance = withSsoToken ssoTestUser
  $ \tok uid run -> do
    res <- run $ checkTokenInstance "" tok defaultInstance
    res `shouldBe` Just ( uid
                        , testUser ^. email, testUser ^. name
                        , []
                        )

case_checkSsoTokenInstance_wrong_instance :: Case ()
case_checkSsoTokenInstance_wrong_instance = withSsoToken ssoTestUser
  $ \tok _uid run -> do
    res <- run $ checkTokenInstance "" tok
      (InstanceID [uuidQ|37f7b338-1e94-4329-9aad-412caa94f179|])
    res `shouldBe` Nothing

case_logout :: Case ()
case_logout = withUserToken testUser $ \tok _uid run -> do
  run $ logOut tok
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_logout_sso :: Case ()
case_logout_sso = withSsoToken ssoTestUser
  $ \tok _uid run -> do
    run $ logOut tok
    res <- run $ checkToken tok
    res `shouldBe` Nothing

case_closeOtherSesions_regular_regular :: Case ()
case_closeOtherSesions_regular_regular = withUserToken testUser $ \tok _uid run -> do
  tok2 <- view token <$> runLogin run testUser
  run $ closeOtherSessions tok2
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions_sso_sso :: Case ()
case_closeOtherSesions_sso_sso = withSsoToken ssoTestUser $ \tok uid run -> do
  let usr = ssoTestUser
      iid = case usr ^. instances of
              [] -> error "no instances"
              (iid:_) -> iid
  res <- run $ createSsoToken uid (usr ^. email) (usr ^. name) iid "" (usr ^. roles)
  let tok2 = res ^. token

  run $ closeOtherSessions tok2

  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions_sso_regular :: Case ()
case_closeOtherSesions_sso_regular = withUserToken testUser $ \tok uid run -> do
  let usr = ssoTestUser
      uidTxt = UUID.toText $ unUserID uid
      iid = case usr ^. instances of
              [] -> error "no instances"
              (iid:_) -> iid
  run $ addInstance (Just iid) "default instance"
  res <- run $ createSsoToken uidTxt (usr ^. email) (usr ^. name) iid "" (usr ^. roles)
  let tok2 = res ^. token

  -- SSO token
  run $ closeOtherSessions tok2

  -- Regular token
  res <- run $ checkToken tok
  case res of
    Nothing -> assertFailure "Token was disabled"
    Just{} -> return ()

case_closeOtherSesions_regular_sso :: Case ()
case_closeOtherSesions_regular_sso = withUserToken testUser $ \tok uid run -> do
  let usr = ssoTestUser
      uidTxt = UUID.toText $ unUserID uid
      iid = case usr ^. instances of
              [] -> error "no instances"
              (iid:_) -> iid
  run $ addInstance (Just iid) "default instance"
  res <- run $ createSsoToken uidTxt (usr ^. email) (usr ^. name) iid "" (usr ^. roles)
  let tok2 = res ^. token

  -- regular token
  run $ closeOtherSessions tok

  -- SSO token
  res <- run $ checkToken tok2
  case res of
    Nothing -> assertFailure "Token was disabled"
    Just{} -> return ()

case_closeOtherSesions_same_session :: Case ()
case_closeOtherSesions_same_session =
  withUserToken testUser $ \_tok uid run -> do
    tok2 <- view token <$> runLogin run testUser
    run $ closeOtherSessions tok2
    res <- run $ checkToken tok2
    res `shouldBe` Just (UUID.toText $ unUserID uid)

adminTests :: ConnectionPool -> TestTree
adminTests pool = testGroup "admin"
  [ it "succeeds when user is admin" $ do
      withUserToken (testUser & roles .~ ["admin"]) $ \tok _uid run -> do
        res <- run $ checkAdmin "" tok
        case res of
          Nothing -> expectationFailure "checkAdmin returned Nothing"
          Just{} -> return ()
  , it "fails when user is not admin" $ do
      withUserToken testUser $ \tok _uid run -> do
        res <- run $ checkAdmin "" tok
        res `shouldBe` Nothing

  , it "fails when user is deactivated" $ do
      withUserToken (testUser & roles .~ ["admin"]) $ \tok uid run -> do
        res1 <- run $ checkAdmin "" tok
        res1 `shouldNotBe` Nothing

        run $ deactivateUser uid DeactivateNow

        res2 <- run $ checkAdmin "" tok
        res2 `shouldBe` Nothing
  ]
  where it n x = testCase n (x pool)


main :: IO ()
main = withTestDB $ \pool -> do
  defaultMain . testGroup "main" $
       [ testCase "create user"                         $ case_create_user                          pool
       , testCase "user check password"                 $ case_user_check_password                  pool
       , testCase "user check password wrong"           $ case_user_check_password_wrong            pool
       , testCase "user email case insensitive"         $ case_user_email_case_insensitive          pool
       , testCase "user change password"                $ case_user_change_password                 pool
       , testCase "user change password old password"   $ case_user_change_password_old_password    pool
       , testCase "changePassword otp"                  $ case_changePassword_otp                   pool
       , testCase "reset password"                      $ case_reset_password                       pool
       , testCase "reset password wrong token"          $ case_reset_password_wrong_token           pool
       , testCase "reset password OTP"                  $ case_reset_password_OTP                   pool
       , testCase "reset password double use"           $ case_reset_password_double_use            pool
       , testCase "reset password expired"              $ case_reset_password_expired               pool
       , testCase "password reset render email"         $ case_password_reset_render_email          pool
       , testCase "password reset render error email"   $ case_password_reset_render_error_email    pool
       , testCase "password reset render email error"   $ case_password_reset_render_email_errors   pool
       , testCase "add user instance"                   $ case_add_user_instance                    pool
       , testCase "remove user instance"                $ case_remove_user_instance                 pool
       , testCase "login"                               $ case_login                                pool
       , testCase "login otp"                           $ case_login_otp                            pool
       , testCase "login otp wrong user"                $ case_login_otp_wrong_user                 pool
       , testCase "checkToken"                          $ case_checkToken                           pool
       , testCase "checkToken bogus"                    $ case_checkToken_bogus                     pool
       , testCase "checkToken token expires"            $ case_checkToken_expired                   pool
       , testCase "checkToken unused token expires"     $ case_checkToken_unused_expired            pool
       , testCase "checkTokenInstance"                  $ case_checkTokenInstance                   pool
       , testCase "checkTokenInstance not member"       $ case_checkTokenInstance_not_member        pool
       , testCase "checkSsoTokenInstance"               $ case_checkSsoTokenInstance                pool
       , testCase "checkSsoTokenInstance wronginstance" $ case_checkSsoTokenInstance_wrong_instance pool
       , testCase "logout"                              $ case_logout_sso                           pool
       , testCase "SSO token logout"                    $ case_logout                               pool
       , testCase "closeOtherSesions regular"           $ case_closeOtherSesions_regular_regular    pool
       , testCase "closeOtherSesions SSO"               $ case_closeOtherSesions_sso_sso            pool
       , testCase "closeOtherSesions SSO and regular"   $ case_closeOtherSesions_sso_regular        pool
       , testCase "closeOtherSesions regular and SSO"   $ case_closeOtherSesions_regular_sso        pool
       , testCase "closeOtherSesions same session"      $ case_closeOtherSesions_same_session       pool
       , adminTests pool
       ]

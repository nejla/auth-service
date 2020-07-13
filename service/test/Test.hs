{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | API tests

module Main where


import           Control.Lens
import qualified Control.Monad.Catch     as Ex
import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.Data
import           Data.IORef
import           Data.Monoid
import qualified Data.Text               as Text
import           Data.Text               (Text)
import           Prelude                 hiding (id)
import qualified Test.QuickCheck.Monadic as QC
import qualified Text.Microstache        as Mustache

import           Test.Hspec.Expectations
import           Test.Tasty.HUnit        hiding (assertFailure)
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Backend
import           Config                  (defaultPwResetTemplate)
import           PasswordReset
import qualified Persist.Schema          as DB
import           Types

import           Test.Common

newtype AssertionFailed = AssertionFailed String deriving (Typeable)

instance Show AssertionFailed where
  show (AssertionFailed e) = "Assertion Failed: " <>  e

instance Ex.Exception AssertionFailed

assertFailure :: Ex.MonadThrow m => String -> m a
assertFailure = Ex.throwM  . AssertionFailed

--------------------------------------------------------------------------------
-- Add User
--------------------------------------------------------------------------------

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
testUserOtp = testUser & phone .~ (Just $ Phone "12345")

withUser :: AddUser -> (UserID -> (forall a. API a -> IO a) -> IO ()) -> IO ()
withUser usr f = withRunAPI Nothing $ \run -> do
  mbRes <- run $ createUser usr
  case mbRes of
    Nothing -> assertFailure "could not create user"
    Just uid -> f uid run

withUserOTP ::
     AddUser
  -> (UserID -> IO (Maybe Text) -> (forall a. API a -> IO a) -> IO ())
  -> IO ()
withUserOTP usr f = do
  otpRef <- newIORef Nothing
  let otpHandler _ = liftIO . writeIORef otpRef . Just
      readOTP = do
        res <- readIORef otpRef
        writeIORef otpRef Nothing
        return res
  withRunAPI (Just otpHandler) $ \run -> do
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
    res <- run $ login Login { loginUser = addUserEmail
                             , loginPassword = addUserPassword
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otp <- getOtp
    res' <- run $ login Login { loginUser = addUserEmail
                              , loginPassword = addUserPassword
                              , loginOtp = Just $ Password otp
                              }
    case res' of
      Left e -> assertFailure $ "Failed login with OTP " <> (show e)
      Right r -> return $ returnLoginToken r

case_create_user :: IO ()
case_create_user = withUser testUser $ \_uid run -> do
  mbUsr <- run . getUserByEmail $ testUser ^. email
  case mbUsr of
    Nothing -> assertFailure "Did not get user"
    Just usr -> checkUser usr testUser

case_user_check_password :: IO ()
case_user_check_password = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> assertFailure "check password failed"
    Right _ -> return ()

case_user_check_password_wrong :: IO ()
case_user_check_password_wrong = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (Password "bogus")
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Accepted bogus password"

--------------------------------------------------------------------------------
-- Password Changes
--------------------------------------------------------------------------------

case_user_change_password :: IO ()
case_user_change_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword uid (Password "newPassword")
  res <- run $ checkUserPassword (testUser ^. email) (Password "newPassword")
  case res of
    Left _e -> assertFailure "check password fails"
    Right _ -> return ()

case_user_change_password_old_password :: IO ()
case_user_change_password_old_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword uid (Password "newpassword")
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Could still use old password"

case_changePassword_otp :: IO ()
case_changePassword_otp =
  withUserOTP testUserOtp $ \uid getOtp run -> do
    tok <- loginOTP run getOtp testUserOtp
    res <-
      run $
      changePassword
        tok
        ChangePassword
        { changePasswordOldPasword = testUserOtp ^. password
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
        { changePasswordOldPasword = testUserOtp ^. password
        , changePasswordNewPassword = "pwd2"
        , changePasswordOtp = Just $ Password otp
        }
    res' `shouldBe` Right()
    return ()

--------------------------------------------------------------------------------
-- Reset Password
--------------------------------------------------------------------------------

case_reset_password :: IO ()
case_reset_password = withUser testUser $ \uid run -> do
  tok <- run $ createResetToken 60 uid
  _ <- run $ resetPassword tok "newPwd" Nothing
  _ <- run $ checkUserPassword (testUser ^. email) "newPwd"
  return ()

case_reset_password_wrong_token :: IO ()
case_reset_password_wrong_token =
  withRunAPI Nothing $ \run -> do
    run (resetPassword "BogusToken" "newPwd" Nothing) `shouldReturn`
      (Left ChangePasswordTokenError)
    return ()

case_reset_password_OTP :: IO ()
case_reset_password_OTP = withUserOTP testUserOtp $ \uid getOtp run -> do
  tok <- run $ createResetToken 60 uid
  res <- run (resetPassword tok "newPwd" Nothing)
  res `shouldBe` (Left $ ChangePasswordLoginError LoginErrorOTPRequired)
  Just otp <- getOtp
  run (resetPassword tok "newPwd" $ Just (Password otp))
  return ()

case_reset_password_double_use :: IO ()
case_reset_password_double_use =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken 60 uid
    run $ resetPassword tok "newPwd" Nothing
    run (resetPassword tok "newPwd2" Nothing) `shouldReturn`
      (Left ChangePasswordTokenError)
    return ()

case_reset_password_expired :: IO ()
case_reset_password_expired =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken (-60) uid
    run (resetPassword tok "newPwd" Nothing) `shouldReturn`
      (Left ChangePasswordTokenError)
    return ()

testEmailData :: EmailData
testEmailData =
  EmailData
  { emailDataLink = "http://localhost/reset/abc"
  , emailDataSiteName = "test.site.com"
  , emailDataExpirationTime = "24 hours"
  }

case_password_reset_render_email :: IO ()
case_password_reset_render_email = do
  renderedEmail <-
    runNoLoggingT $
    renderEmail
      testEmailConfig
      (testEmailConfig ^. pWResetTemplate)
      (Just "tok123abc")
  renderedEmail `shouldBe`
    "please click on http://localhost/reset?token=tok123abc"

case_password_reset_render_error_email :: IO ()
case_password_reset_render_error_email = do
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
case_password_reset_render_email_errors :: IO ()
case_password_reset_render_email_errors = do
  let Right tmpl = Mustache.compileMustacheText "test" "{{bogus}}"
  runNoLoggingT (renderEmail testEmailConfig tmpl Nothing)
    `shouldThrow` (== EmailRenderError)

-- Check that we can render the default template (i.e. there are no missing
-- variables)
case_password_reset_default_template :: IO ()
case_password_reset_default_template = do
  _ <-
    runStderrLoggingT $ renderEmail testEmailConfig defaultPwResetTemplate Nothing
  return ()

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

case_add_user_instance :: IO ()
case_add_user_instance = withUser testUser $ \uid run -> do
  iid <- run $ addInstance Nothing "instance1"
  run $ addUserInstance uid iid
  iids <- run $ getUserInstances uid
  iids ^.. each . id  `shouldBe` [iid]

case_remove_user_instance :: IO ()
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
  res <- run $ login Login{ loginUser = usr ^. email
                          , loginPassword = usr ^. password
                          , loginOtp = Nothing
                          }
  case res of
    Left _e -> assertFailure "Could not login"
    Right rl -> return rl

case_login :: IO ()
case_login = withUser testUser $ \_uid run -> do
  _ <- runLogin run testUser
  return ()

case_login_otp :: IO ()
case_login_otp =  withUserOTP testUserOtp $ \_uid getOtp run -> do
    res <- run $ login Login { loginUser = testUserOtp ^. email
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otp <- getOtp
    res' <- run $ login Login { loginUser = testUserOtp ^. email
                              , loginPassword = testUserOtp ^. password
                              , loginOtp = Just $ Password otp
                              }
    case res' of
      Left e -> assertFailure $ "Failed login with OTP " <> (show e)
      Right _ -> return ()

case_login_otp_wrong_user :: IO ()
case_login_otp_wrong_user =
  withUserOTP testUserOtp $ \_uid getOtp run -> do
    _ <- run . createUser $ testUserOtp & email .~ "user2@spam.please"

    -- First, log in alternative user and get OTP
    res <- run $ login Login { loginUser = "user2@spam.please"
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired
    Just otpUser2 <- getOtp
    -- Now log in test user, ignore OTP
    res <- run $ login Login { loginUser = testUserOtp ^. email
                             , loginPassword = testUserOtp ^. password
                             , loginOtp = Nothing
                             }
    res `shouldBe` Left LoginErrorOTPRequired


    -- Finally, try to log in test user with alternative user's OTP
    res' <- run $ login Login { loginUser = testUserOtp ^. email
                              , loginPassword = testUserOtp ^. password
                              , loginOtp = Just $ Password otpUser2
                              }

    res' `shouldBe` (Left LoginErrorFailed)

withUserToken :: AddUser
              -> (B64Token -> UserID -> (forall a. API a -> IO a) -> IO ())
              -> IO ()
withUserToken usr f = withUser usr $ \uid run -> do
  res <- runLogin run usr
  f (res ^. token) uid run


case_checkToken :: IO ()
case_checkToken = withUserToken testUser $ \tok uid run -> do
  res <- run $ checkToken tok
  res `shouldBe` Just uid

case_checkToken_bogus :: IO ()
case_checkToken_bogus = withUser testUser $ \_uid run -> do
  let tok = B64Token "bogus"
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_checkTokenInstance :: IO ()
case_checkTokenInstance = withUserToken testUser $ \tok uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  run $ addUserInstance uid iid
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Just (uid, addUserEmail testUser, addUserName testUser)

case_checkTokenInstance_not_member :: IO ()
case_checkTokenInstance_not_member = withUserToken testUser $ \tok _uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Nothing


case_logout :: IO ()
case_logout = withUserToken testUser $ \tok _uid run -> do
  run $ logOut tok
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions :: IO ()
case_closeOtherSesions = withUserToken testUser $ \tok _uid run -> do
  tok2 <- view token <$> runLogin run testUser
  run $ closeOtherSessions tok2
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions_same_session :: IO ()
case_closeOtherSesions_same_session =
  withUserToken testUser $ \_tok uid run -> do
    tok2 <- view token <$> runLogin run testUser
    run $ closeOtherSessions tok2
    res <- run $ checkToken tok2
    res `shouldBe` Just uid


main :: IO ()
main = $defaultMainGenerator

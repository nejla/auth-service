{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module PasswordReset where

import Backend
import Control.Exception qualified as Ex (ErrorCall (..))
import Control.Lens hiding (from)
import Control.Monad.Catch qualified as Ex
import Control.Monad.Logger qualified as Log
import Control.Monad.Trans
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time.Clock
import Data.Traversable qualified as Traversable
import Logging
import Monad
import NejlaCommon (viewState)
import Network.Mail.Mime qualified as Mail
import Text.Microstache qualified as Mustache
import Types
import Prelude hiding (id)

-- | Create an Aeson object from password reset email data
fromEmailData :: EmailData -> Aeson.Value
fromEmailData emailData =
  Aeson.object
    [ "token" .= (emailData ^. Types.token)
    , "expirationTime" .= emailData ^. expirationTime
    ]
 where
  infix 0 .=
  (.=) = (Aeson..=)

renderEmail
  :: (Log.MonadLogger m, Ex.MonadThrow m)
  => EmailConfig
  -> Mustache.Template
  -> Maybe PwResetToken
  -> m LText.Text
renderEmail cfg tmpl mbToken =
  let emailData =
        EmailData
          { emailDataExpirationTime = Text.pack $ show $ cfg ^. resetLinkExpirationTime
          , emailDataToken = fromMaybe "" mbToken
          }
      (warnings, result) = Mustache.renderMustacheW tmpl $ fromEmailData emailData
   in case warnings of
        [] -> return result
        (_ : _) -> do
          logError $
            "Could not render email, Microstache warnings: "
              <> Text.pack (show warnings)
          Ex.throwM EmailRenderError

sendEmail
  :: EmailConfig
  -> Mustache.Template
  -> Email
  -> Text
  -> Maybe PwResetToken
  -> API Bool
sendEmail cfg tmpl (Email toAddress) subject token = do
  let sendmailCfg = cfg ^. sendmail
  body <- renderEmail cfg tmpl token
  let plainBody = "Please see the HTML attachment."
      to = Mail.Address Nothing toAddress
  let mail =
        Mail.addPart [Mail.plainPart plainBody, Mail.htmlPart body] $
          (Mail.emptyMail (cfg ^. from))
            { Mail.mailHeaders = [("Subject", subject)]
            , Mail.mailTo = [to]
            }
  mbError <-
    liftIO . Ex.try $
      Mail.renderSendMailCustom
        (sendmailCfg ^. path)
        (sendmailCfg ^. arguments)
        mail
  case mbError of
    Left (Ex.ErrorCall msg) -> do
      logError $ "Error sending password reset mail: " <> Text.pack msg
      return False
    Right () -> return True

sendPasswordResetEmail :: EmailConfig -> Email -> PwResetToken -> API Bool
sendPasswordResetEmail cfg toAddress token = do
  logES $ PasswordResetRequested toAddress
  sendEmail cfg (cfg ^. pWResetTemplate) toAddress "Password reset" (Just token)

sendPasswordResetUnknownEmail :: EmailConfig -> Email -> API Bool
sendPasswordResetUnknownEmail cfg toAddress = do
  logES $ PasswordResetRequested toAddress
  sendEmail cfg (cfg ^. pWResetUnknownTemplate) toAddress "Password reset" Nothing

passwordResetRequest :: Email -> API Bool
passwordResetRequest userMail =
  viewState (config . email) >>= \case
    Nothing -> Ex.throwM EmailErrorNotConfigured
    Just cfg -> do
      let expirationHours = cfg ^. resetLinkExpirationTime
      mbUser <- getUserByEmail userMail
      mbToken <-
        Traversable.forM mbUser $ \uid ->
          createResetToken (hours expirationHours) (uid ^. uuid)
      case mbToken of
        Nothing -> sendPasswordResetUnknownEmail cfg userMail
        Just tok -> sendPasswordResetEmail cfg userMail tok
 where
  hours :: Int -> NominalDiffTime
  hours hs = fromIntegral $ hs * 6 * 60

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Persist.Migration
  ( doMigrate
  ) where

import NejlaCommon.Persistence.Migration as M

migrations :: [Migration]
migrations =
  [ Migration { expect = Nothing -- No migrations present
              , to = "1"
              , description = "Initial version"
              , script = do
                  schemaEmptyP "public" >>= \case
                    True -> do -- Database not initialized at all
                      rawExecute $(sqlFile "src/Persist/migrations/01-initial.sql") []
                    False -> -- Database _was_ initialized, but schema
                             -- versionioning wasn't in use
                      return ()
              }
  , Migration { expect = Just "1"
              , to = "2"
              , description = "Case insenstivie email addresses"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/02-ci-emails.sql") []
              }
  , Migration { expect = Just "2"
              , to = "3"
              , description = "Add \"deactive\" field to \"user\" relation\""
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/03-user-deactivation.sql") []
              }
  , Migration { expect = Just "3"
              , to = "4"
              , description = "Add audit_log table"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/04-add-audit-log.sql") []
              }
  , Migration { expect = Just "4"
              , to = "5"
              , description = "Add login_attempt table"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/05-login-attempt.sql") []
              }
  , Migration { expect = Just "5"
              , to = "6"
              , description = "Remove personal information from audit log"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/06-audit-log-remove-identifying-info.sql") []
              }
  , Migration { expect = Just "6"
              , to = "7"
              , description = "Add SSO tables"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/07-sso.sql") []
              }
  , Migration { expect = Just "7"
              , to = "8"
              , description = "Add SSO request id table"
              , script = rawExecute
                           $(sqlFile "src/Persist/migrations/08-saml-request-id.sql") []
              }
  ]

doMigrate :: M ()
doMigrate = M.migrate $(gitHash) migrations

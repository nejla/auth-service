{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Persist.Schema (
  module Persist.Schema,
  uuid,
) where

import Control.Lens
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.TH
import NejlaCommon
import Persist.Stage ()
import Types

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  , mkForeignInstances
  ]
  $(persistFileWith lowerCaseSettings "src/schema")

makeLensesWith camelCaseFields ''User
makeLensesWith camelCaseFields ''Instance
makeLensesWith camelCaseFields ''UserOtp
makeLensesWith camelCaseFields ''UserInstance
makeLensesWith camelCaseFields ''Token

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Persist.Schema
  ( module Persist.Schema
  , uuid
  ) where

import Control.Lens
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.TH

import Types
import Persist.Stage ()

import NejlaCommon

share [ mkPersist sqlSettings
      , mkMigrate "migrateAll", mkForeignInstances]
    $(persistFileWith lowerCaseSettings "src/schema")

makeLensesWith camelCaseFields ''User
makeLensesWith camelCaseFields ''Instance
makeLensesWith camelCaseFields ''UserOtp
makeLensesWith camelCaseFields ''UserInstance
makeLensesWith camelCaseFields ''Token

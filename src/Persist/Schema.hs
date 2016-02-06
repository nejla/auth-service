-- Copyright © 2015-2016 Nejla AB. All rights reserved.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Persist.Schema where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.TH

import Types
import Persist.Stage

share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "src/schema")

makeLensesWith camelCaseFields ''User
makeLensesWith camelCaseFields ''Instance
makeLensesWith camelCaseFields ''UserOtp
makeLensesWith camelCaseFields ''UserInstance
makeLensesWith camelCaseFields ''Token

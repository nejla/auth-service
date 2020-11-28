{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Monad
  ( module Monad
  , AuditEvent(..)
  )
where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Default         (def)
import           Database.Persist.Sql
import           Language.Haskell.TH
import qualified NejlaCommon          as NC

import           Types
import           Audit

--------------------------------------------------------------------------------
-- Api Monad -------------------------------------------------------------------
--------------------------------------------------------------------------------

type Ctx = ApiState
data ApiState = ApiState { apiStateConfig :: Config
                         , apiStateAuditSource :: AuditSource
                         }

makeLensesWith camelCaseFields ''ApiState

type API a = NC.App ApiState 'NC.Privileged 'NC.ReadCommitted a

-- newtype API a = API { unAPI :: ReaderT ApiState IO a }
--               deriving ( Functor, Applicative, Monad, MonadIO
--                        , MonadThrow, MonadCatch)

runDB :: ReaderT SqlBackend IO a -> API a
runDB = NC.db'

getConfig ::  Lens' Config a -> API a
getConfig g = NC.viewState $ config . g

runAPI :: ConnectionPool -> ApiState -> API a -> IO a
runAPI pool st m =
  NC.runApp' (def & NC.useTransactionLevels .~ (st ^. config . useTransactionLevels))
             pool st m

audit :: AuditEvent -> API ()
audit event = do
  src <- NC.viewState auditSource
  _ <- runDB $ addAuditLog src event
  return ()

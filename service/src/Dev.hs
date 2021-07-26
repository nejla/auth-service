-- Helpers for developing this application (for ghci)

module Dev where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Database.Persist.Postgresql
import           Types
import           Monad
import           Audit                       (AuditSource(AuditSourceManual))

import qualified SignedAuth

mkDevConf :: IO ApiState
mkDevConf = do
  noncePool <- SignedAuth.newNoncePool
  return ApiState
    { apiStateConfig = Config
      { configTimeout              = Nothing
      , configTokenUnusedTimeout   = Nothing
      , configMaxAttempts          = 5
      , configAttemptsTimeframe    = 60
      , configOTPLength            = 1
      , configOTPTimeoutSeconds    = 3600
      , configTFARequired          = False
      , configOtp                  = Nothing
      , configUseTransactionLevels = False
      , configEmail                = Nothing
      , configAccountCreation      = AccountCreationConfig
        { accountCreationConfigEnabled = True
        , accountCreationConfigDefaultInstances = []
        }
      }
    , apiStateAuditSource = AuditSourceManual
    , apiStateNoncePool = noncePool
  }


runApiDev :: ConnectionString -> API a -> IO a
runApiDev cstr m = runStderrLoggingT $withPostgresqlPool cstr 3 $ \pool -> do
  devConf <- liftIO mkDevConf
  liftIO $ runAPI pool devConf m

{-# LANGUAGE StrictData #-}

module SignedAuth.Nonce where

import           Data.Bits
import           Data.IORef
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Word

-- How far back we keep nonces
nonceStoreTimeout :: NominalDiffTime
nonceStoreTimeout = 5 -- seconds

-- Nonces older than this are rejected. Has to be < nonceStoreTimeout or we can
-- accept nonces that we already "forgot" about. Difference between the two is
-- the grace period we have for handling the nonce check.
nonceRejectTimeout :: NominalDiffTime
nonceRejectTimeout = 4 -- seconds

type Nonce = Word64

--------------------------------------------------------------------------------
-- Creating Nonces -------------------------------------------------------------
--------------------------------------------------------------------------------

type NoncePool = IORef Nonce

newNoncePool :: IO NoncePool
newNoncePool = do
  -- Start nonce counter using current time to avoid replaying nonces
  start <- getPOSIXTime
    -- Seconds since epoch times takes roughtly 31 bits to store
    -- log_2(50 * 365 * 24 * 60 * 60) ~ 30.6 with 32 being enough for 130 years
    -- so we can take the lower 32 bit for counting
  newIORef $ round start {-seconds -} `shiftL` 32

mkNonce :: NoncePool -> IO Nonce
mkNonce pool = do
  atomicModifyIORef pool (\x -> (x+1,x+1))

--------------------------------------------------------------------------------
-- Checking Nonces -------------------------------------------------------------
--------------------------------------------------------------------------------
-- Instead of a "sliding" window we have two windows in a frame, one going from
-- (split - timout) to split and one going from split to current-time. We always
-- add new nonces to the current window, ensuring that all nonces in the old
-- window are at least (now - split) old. Once split is older than the timeout
-- (and hence all nonces in the old window are older than the timeout) we
-- discard the old window, cycle the new window to become the old frame and
-- start an empty frame as the current frame.
--
-- This ensures that we find duplicate nonces _at least_ as far back as (now -
-- timeout), possible further.
data NonceFrame =
  NonceFrame { nfOldNonces :: Set Nonce -- nonces we got before the split
             , nfSplit :: POSIXTime -- when we split
             , nfCurrentNonces :: Set Nonce -- current frame
             } deriving (Eq, Show)

mkFrame :: POSIXTime -> NonceFrame
mkFrame now = NonceFrame { nfOldNonces = mempty
                         , nfSplit = now + nonceRejectTimeout
                         -- Reject any nonce that was generated before we started
                         -- to avoid replays after a restart
                         , nfCurrentNonces = mempty
                         }


type Frame = IORef NonceFrame

newFrame :: IO Frame
newFrame = do
  now <- getPOSIXTime
  newIORef (NonceFrame Set.empty now Set.empty)

data Verdict = RejectOld | RejectSeen | Accept deriving (Eq, Show)

-- | Cycle frames, check if nonce is present and insert if it is not
updateCheckNonceFrame
  :: POSIXTime -- ^ Nonce timestamp
  -> Nonce -- ^ Nonce to handle
  -> NonceFrame -- ^ Frames to look nonce up
  -> (NonceFrame, Verdict)
updateCheckNonceFrame timestamp nonce frames0@(NonceFrame old0 split0 current0)
  -- Nonce timestamp is older than our record
  | timestamp < split0 - nonceRejectTimeout = (frames0, RejectOld)
  -- Nonce is in seen set
  | Set.member nonce old0 || Set.member nonce current0 = (frames0, RejectSeen)
  | otherwise =
      let cutoff = timestamp - nonceStoreTimeout
      -- Cycle frames if necessary
          (NonceFrame old1 split1 current1) =
            if split0 < cutoff
              then NonceFrame current0 timestamp Set.empty
              else frames0
      in (NonceFrame old1 split1 (Set.insert nonce current1), Accept)


handleNonce :: Frame -> POSIXTime -> Nonce -> IO Verdict
handleNonce ref timestamp nonce =
  atomicModifyIORef ref $ updateCheckNonceFrame timestamp nonce

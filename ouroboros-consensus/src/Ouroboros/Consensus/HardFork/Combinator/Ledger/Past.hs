{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Past (
    PastLedgerState(..)
  , pastLedgerTransition
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import qualified Ouroboros.Consensus.HardFork.History as History

{-------------------------------------------------------------------------------
  Past ledger state
-------------------------------------------------------------------------------}

data PastLedgerState blk = PastLedgerState {
      -- | Start of this era
      pastLedgerStart :: !History.Bound

      -- | End of this era
    , pastLedgerEnd   :: !History.Bound
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoUnexpectedThunks)

pastLedgerTransition :: PastLedgerState blk -> EpochNo
pastLedgerTransition = History.boundEpoch . pastLedgerEnd

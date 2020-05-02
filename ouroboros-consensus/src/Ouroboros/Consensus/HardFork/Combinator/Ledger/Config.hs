{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Config (
    HardForkLedgerConfig(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Ouroboros.Consensus.HardFork.History as History

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

data HardForkLedgerConfig xs = HardForkLedgerConfig {
      hardForkLedgerConfigPerEra :: !(PerEraLedgerConfig xs)
    , hardForkLedgerConfigStart  :: !(History.Bound)
    }
  deriving (Generic)

instance CanHardFork xs => NoUnexpectedThunks (HardForkLedgerConfig xs)

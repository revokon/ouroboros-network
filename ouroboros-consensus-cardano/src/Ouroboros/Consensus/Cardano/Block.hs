{-# LANGUAGE DataKinds #-}
module Ouroboros.Consensus.Cardano.Block (
    CardanoBlock
  , CardanoEras
  ) where

import           Ouroboros.Consensus.HardFork.Combinator.Block (HardForkBlock)

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

type CardanoEras = '[ByronBlock, ShelleyBlock TPraosStandardCrypto]

type CardanoBlock = HardForkBlock CardanoEras

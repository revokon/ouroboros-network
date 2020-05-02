{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Abstract (
    SingleEraBlock(..)
  , proxySingle
  , CanHardFork(..)
    -- * Re-exports
  , IsNonEmpty(..)
  , NonEmpty(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Proxy
import           Data.SOP
import           Data.Typeable

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.HardFork.History (EraParams)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Translation
import           Ouroboros.Consensus.HardFork.Combinator.Util.SOP

-- | Blocks from which we can assemble a hard fork
class ( LedgerSupportsProtocol blk
      , ApplyTx blk
      , HasTxId (GenTx blk)
      , QueryLedger blk
        -- Only needed for the PBFT hack; see 'rewindConsensusState'
      , Serialise (HeaderHash blk)
      ) => SingleEraBlock blk where
  singleEraParams     :: proxy blk -> LedgerConfig blk -> EraParams
  singleEraTransition :: LedgerConfig blk -> LedgerState blk -> Maybe EpochNo
  singleEraInfo       :: proxy blk -> SingleEraInfo blk

proxySingle :: Proxy SingleEraBlock
proxySingle = Proxy

class (All SingleEraBlock xs, Typeable xs, NonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation     :: EraTranslation     xs
  hardForkEraTransitionCheck :: EraTransitionCheck xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation     = trivialEraTranslation
  hardForkEraTransitionCheck = trivialEraTransitionCheck

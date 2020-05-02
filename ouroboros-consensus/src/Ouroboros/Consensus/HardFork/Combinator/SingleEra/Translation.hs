{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Translation (
    -- * Translate from one era to the next
    TranslateEraLedgerState(..)
  , TranslateEraConsensusState(..)
  , EraTranslation(..)
  , trivialEraTranslation
    -- * Check that eras line up
  , CheckTransition(..)
  , EraTransitionCheck(..)
  , trivialEraTransitionCheck
  ) where

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

newtype TranslateEraLedgerState blk blk' = TranslateEraLedgerState {
      translateLedgerStateWith :: LedgerConfig blk
                               -> LedgerConfig blk'
                               -> EpochNo
                               -> LedgerState blk
                               -> LedgerState blk'
    }

newtype TranslateEraConsensusState blk blk' = TranslateEraConsensusState {
      translateConsensusStateWith :: ConsensusConfig (BlockProtocol blk)
                                  -> ConsensusConfig (BlockProtocol blk')
                                  -> ConsensusState (BlockProtocol blk)
                                  -> ConsensusState (BlockProtocol blk')
    }

data EraTranslation xs = EraTranslation {
      translateLedgerState    :: InPairs TranslateEraLedgerState xs
    , translateConsensusState :: InPairs TranslateEraConsensusState xs
    }
  deriving NoUnexpectedThunks
       via OnlyCheckIsWHNF "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState    = PNil
    , translateConsensusState = PNil
    }

{-------------------------------------------------------------------------------
  Check that the transition lines up
-------------------------------------------------------------------------------}

data CheckTransition blk blk' = CheckTransition {
      -- | The prev-hash of next era must line up wih tip hash of current
      checkTransitionWith :: HeaderHash blk -> ChainHash blk' -> Bool
    }

newtype EraTransitionCheck xs = EraTransitionCheck {
      getCheckEraTransition :: InPairs CheckTransition xs
    }

trivialEraTransitionCheck :: EraTransitionCheck '[blk]
trivialEraTransitionCheck = EraTransitionCheck PNil

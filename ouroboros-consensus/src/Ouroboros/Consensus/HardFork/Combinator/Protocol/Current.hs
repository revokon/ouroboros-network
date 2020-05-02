{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.Current (CurrentConsensusState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Current as Current
module Ouroboros.Consensus.HardFork.Combinator.Protocol.Current (
    CurrentConsensusState(..)
  , lift
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract

newtype CurrentConsensusState blk = CurrentConsensusState {
      currentConsensusState :: ConsensusState (BlockProtocol blk)
    }

lift :: Functor f
     => (ConsensusState (BlockProtocol blk) -> f (ConsensusState (BlockProtocol blk)))
     -> CurrentConsensusState blk -> f (CurrentConsensusState blk)
lift f (CurrentConsensusState cur) = CurrentConsensusState <$> f cur

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance SingleEraBlock blk => Show               (CurrentConsensusState blk)
deriving instance SingleEraBlock blk => Eq                 (CurrentConsensusState blk)
deriving instance SingleEraBlock blk => NoUnexpectedThunks (CurrentConsensusState blk)

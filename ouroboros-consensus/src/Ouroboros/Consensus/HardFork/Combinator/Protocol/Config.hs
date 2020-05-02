{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol.Config (
    HardForkProtocol
  , ConsensusConfig(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

data HardForkProtocol (xs :: [*])

data instance ConsensusConfig (HardForkProtocol xs) = HardForkConsensusConfig {
      hardForkConsensusConfigK      :: SecurityParam
    , hardForkConsensusConfigPerEra :: PerEraConsensusConfig xs
    }
  deriving (Generic)

instance CanHardFork xs => NoUnexpectedThunks (ConsensusConfig (HardForkProtocol xs))

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol () where

import           Data.SOP

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
                     (HardForkSelectView (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel as ChainSel
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Config
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.State
                     (HardForkConsensusState, HardForkValidationErr)
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.State as ProtocolState
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ConsensusState (HardForkProtocol xs) = HardForkConsensusState xs
  type ValidationErr  (HardForkProtocol xs) = HardForkValidationErr  xs
  type SelectView     (HardForkProtocol xs) = HardForkSelectView     xs
  type IsLeader       (HardForkProtocol xs) = OneEraIsLeader         xs
  type LedgerView     (HardForkProtocol xs) = OneEraLedgerView       xs
  type ValidateView   (HardForkProtocol xs) = OneEraValidateView     xs

  -- Chain selection

  preferCandidate      = ChainSel.prefer
  compareCandidates    = ChainSel.compare

  -- Operations on the state

  checkIsLeader        = ProtocolState.check
  rewindConsensusState = ProtocolState.rewind
  updateConsensusState = ProtocolState.update

  --
  -- Straight-forward extensions
  --

  -- We can be a leader if we can be a leader in /any/ era
  checkIfCanBeLeader =
        or
      . hcollapse
      . hcmap proxySingle (\(SingleEraConsensusConfig cfg') ->
                               K $ checkIfCanBeLeader cfg')
      . getPerEraConsensusConfig
      . hardForkConsensusConfigPerEra

  -- Security parameter must be equal across /all/ eras
  protocolSecurityParam = hardForkConsensusConfigK

{-------------------------------------------------------------------------------
  BlockSupportsProtocol
-------------------------------------------------------------------------------}

type instance BlockProtocol (HardForkBlock xs) = HardForkProtocol xs

instance CanHardFork xs => BlockSupportsProtocol (HardForkBlock xs) where
  validateView HardForkBlockConfig{..} =
        OneEraValidateView
      . hczipWith proxySingle (SingleEraValidateView .: validateView) cfgs
      . getOneEraHeader
      . getHardForkHeader
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

  selectView HardForkBlockConfig{..} hdr =
        HardForkSelectView (blockNo hdr)
      . OneEraSelectView
      . hczipWith proxySingle (SingleEraSelectView .: selectView) cfgs
      . getOneEraHeader
      $ getHardForkHeader hdr
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

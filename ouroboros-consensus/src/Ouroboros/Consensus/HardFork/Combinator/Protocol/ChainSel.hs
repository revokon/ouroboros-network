{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (HardForkSelectView(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel as ChainSel
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (
    HardForkSelectView(..)
  , prefer
  , compare
  ) where

import           Prelude hiding (compare)
import qualified Prelude

import           Data.Functor.Product
import           Data.SOP

import           Cardano.Slotting.Block

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Config
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match

data HardForkSelectView xs = HardForkSelectView {
      hardForkSelectViewBlockNo :: BlockNo
    , hardForkSelectViewOneEra  :: OneEraSelectView xs
    }

-- | Is this candidate prefered over our own chain?
--
-- We just do the default here and use 'compare'.
prefer :: CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> HardForkSelectView xs
       -> HardForkSelectView xs
       -> Bool
prefer cfg ours cand = compare cfg ours cand == LT

-- | Compare two candidates
--
-- TODO: If the two chains are from different eras, we simply pick the longer
-- one. This may not be okay for all hard fork transitions; we might need to
-- generalize this later.
compare :: forall xs. CanHardFork xs
        => ConsensusConfig (HardForkProtocol xs)
        -> HardForkSelectView xs
        -> HardForkSelectView xs
        -> Ordering
compare HardForkConsensusConfig{..} =
    either same (uncurry different) .: matchView
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    -- If the two views are from the same era, just use 'compareCandidates'
    same :: NS (Product SingleEraSelectView SingleEraSelectView) xs -> Ordering
    same = hcollapse . hczipWith proxySingle compareCandidates' cfgs

    -- If the two tips are in different eras, just compare chain length
    different :: BlockNo -> BlockNo -> Ordering
    different = Prelude.compare

compareCandidates' :: SingleEraBlock blk
                   => SingleEraConsensusConfig blk
                   -> Product SingleEraSelectView SingleEraSelectView blk
                   -> K Ordering blk
compareCandidates' (SingleEraConsensusConfig cfg)
                   (Pair (SingleEraSelectView view1)
                         (SingleEraSelectView view2)) = K $
    compareCandidates cfg view1 view2

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

matchView :: HardForkSelectView xs
          -> HardForkSelectView xs
          -> Either (NS (Product SingleEraSelectView SingleEraSelectView) xs)
                    (BlockNo, BlockNo)
matchView cand1 cand2 =
    case matchNS (getOneEraSelectView $ hardForkSelectViewOneEra cand1)
                 (getOneEraSelectView $ hardForkSelectViewOneEra cand2) of
      Right matched  -> Left matched
      Left _mismatch -> Right ( hardForkSelectViewBlockNo cand1
                              , hardForkSelectViewBlockNo cand2
                              )

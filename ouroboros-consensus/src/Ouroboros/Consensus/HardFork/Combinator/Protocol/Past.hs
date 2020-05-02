{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.Past (PastConsensusState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Past as Past
module Ouroboros.Consensus.HardFork.Combinator.Protocol.Past (
    PastConsensusState(..)
  , tick
  ) where

import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract

-- | Past consensus state
--
-- We record for each past era how many blocks have been applied to /any/
-- subsequent era. Here is an example with @k = 3@:
--
-- >         |                |       3         2      1      0
-- >         |            3   |       2         1      0
-- >         |    3       2   |       1         0
-- >      3  |    2       1   |       0
-- >      2  |    1       0   |
-- >      1  |    0           |
-- >   1  0  |                |
-- >   0     |                |
-- >   A  A  | A,B->A,B  A,B  | A,B,C->A,B,C  A,B,C  A,B,C  A,B,C
-- >         | 0->  1    2    | 3,0  ->3,1    4,2    5,3    6,4
-- >
-- >                                          (*)           (**)
--
-- Whenever the count exceeds @k@ in any era, that era can be GCed. For example,
-- era A can be GCed at point (*), and era B can be GCed at point (**).
data PastConsensusState blk =
    PastConsensusState !(ConsensusState (BlockProtocol blk)) !Word64

    -- | Past consensus state not available anymore
    --
    -- After @k@ blocks have been applied, we are sure that we don't need
    -- the old consensus state anymore and so we don't need to keep it around.
  | PastConsensusStateGCed
  deriving (Generic)

tick :: SecurityParam -> PastConsensusState blk -> PastConsensusState blk
tick (SecurityParam k) = \case
    PastConsensusState st n | n < k ->
      PastConsensusState st (n + 1)
    _otherwise ->
      PastConsensusStateGCed

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving stock    instance SingleEraBlock blk => Show               (PastConsensusState blk)
deriving stock    instance SingleEraBlock blk => Eq                 (PastConsensusState blk)
deriving anyclass instance SingleEraBlock blk => NoUnexpectedThunks (PastConsensusState blk)

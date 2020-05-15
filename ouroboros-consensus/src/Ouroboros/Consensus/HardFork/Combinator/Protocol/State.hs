{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Protocol.State (HardForkConsensusState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.State as ProtocolState
module Ouroboros.Consensus.HardFork.Combinator.Protocol.State (
    HardForkConsensusState(..)
  , HardForkValidationErr(..)
  , check
  , rewind
  , update
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Functor.Product
import           Data.SOP
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util ((..:), (.:))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Config
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Current
                     (CurrentConsensusState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Current as Current
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Past
                     (PastConsensusState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Past as Past
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Align (..), RestoreTip (..), Telescope (..),
                     UpdateTip (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

newtype HardForkConsensusState xs = HardForkConsensusState {
      getHardForkConsensusState :: Telescope PastConsensusState CurrentConsensusState xs
    }

{-------------------------------------------------------------------------------
  Supporting types
-------------------------------------------------------------------------------}

data HardForkValidationErr xs =
    -- | Validation error from one of the eras
    HardForkValidationErrFromEra (OneEraValidationErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkValidationErrWrongEra (MismatchEraInfo xs)
  deriving (Generic)

{-------------------------------------------------------------------------------
  Leader check
-------------------------------------------------------------------------------}

check :: forall m xs. (MonadRandom m, CanHardFork xs)
      => ConsensusConfig (HardForkProtocol xs)
      -> Ticked (OneEraLedgerView xs)
      -> HardForkConsensusState xs
      -> m (Maybe (OneEraIsLeader xs))
check cfg@HardForkConsensusConfig{..}
         (Ticked slot (OneEraLedgerView ledgerView))
         (HardForkConsensusState consensusState) =
    fmap aux $ hsequence' . Telescope.tip $
      Telescope.align
        (alignConsensus cfg)
        (hczipWith proxySingle (fn .: checkOne slot) cfgs ledgerView)
        consensusState
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    aux :: NS (Maybe :.: SingleEraIsLeader) xs -> Maybe (OneEraIsLeader xs)
    aux ns = hcollapse (hzipWith (K .: injectProof) injections ns)

checkOne :: (MonadRandom m, SingleEraBlock blk)
         => SlotNo
         -> SingleEraConsensusConfig            blk
         -> SingleEraLedgerView                 blk
         -> CurrentConsensusState               blk
         -> (m :.: Maybe :.: SingleEraIsLeader) blk
checkOne slot
         (SingleEraConsensusConfig cfg)
         (SingleEraLedgerView ledgerView)
         (CurrentConsensusState consensusState) = Comp . fmap Comp $
     fmap (fmap SingleEraIsLeader) $
       checkIsLeader
         cfg
         (Ticked slot ledgerView)
         consensusState

{-------------------------------------------------------------------------------
  Rolling forward an backward
-------------------------------------------------------------------------------}

rewind :: CanHardFork xs
       => Serialise (HeaderHash hdr)
       => ConsensusConfig (HardForkProtocol xs)
       -> HardForkConsensusState xs
       -> Point hdr
       -> Maybe (HardForkConsensusState xs)
rewind HardForkConsensusConfig{..}
       (HardForkConsensusState consensusState)
       p = HardForkConsensusState <$>
    Telescope.retract
      (hcliftA proxySingle updateTip cfgs)
      (hpure restoreTip)
      consensusState
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    updateTip :: SingleEraBlock blk
              => SingleEraConsensusConfig blk
              -> UpdateTip CurrentConsensusState blk
    updateTip (SingleEraConsensusConfig cfg) = UpdateTip $
      Current.lift $ flip (rewindConsensusState cfg) p

    restoreTip :: RestoreTip PastConsensusState CurrentConsensusState blk
    restoreTip = RestoreTip $ \case
      PastConsensusState cur _ -> Just $ CurrentConsensusState cur
      PastConsensusStateGCed   -> Nothing

update :: forall xs. CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> Ticked (OneEraLedgerView xs)
       -> OneEraValidateView xs
       -> HardForkConsensusState xs
       -> Except (HardForkValidationErr xs) (HardForkConsensusState xs)
update cfg@HardForkConsensusConfig{..}
       (Ticked slot (OneEraLedgerView ledgerView))
       (OneEraValidateView validateView)
       (HardForkConsensusState consensusState) = HardForkConsensusState <$>
    case Match.matchNS validateView ledgerView of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
                       Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
          hsequence'
        . Telescope.hfirst (Past.tick hardForkConsensusConfigK)
        . Telescope.align
            (alignConsensus cfg)
            (hczipWith3 proxySingle (fn ..: updateEra slot) cfgs injections matched)
        $ consensusState
  where
    cfgs  = getPerEraConsensusConfig hardForkConsensusConfigPerEra

updateEra :: forall xs blk. SingleEraBlock blk
          => SlotNo
          -> SingleEraConsensusConfig                                      blk
          -> Injection SingleEraValidationErr xs                           blk
          -> Product SingleEraValidateView SingleEraLedgerView             blk
          -> CurrentConsensusState                                         blk
          -> (Except (HardForkValidationErr xs) :.: CurrentConsensusState) blk
updateEra slot
          (SingleEraConsensusConfig cfg)
          injectErr
          (Pair (SingleEraValidateView validateView)
                (SingleEraLedgerView ledgerView))
          (CurrentConsensusState consensusState) = Comp $
    withExcept (injectValidationErr injectErr) $
      fmap CurrentConsensusState $
        updateConsensusState
          cfg
          (Ticked slot ledgerView)
          validateView
          consensusState

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => SingleEraLedgerView blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

-- | Align the consensus state with the ledger view
alignConsensus :: forall xs. CanHardFork xs
               => ConsensusConfig (HardForkProtocol xs)
               -> InPairs (Align PastConsensusState CurrentConsensusState) xs
alignConsensus HardForkConsensusConfig{..} =
    InPairs.requiring
      (getPerEraConsensusConfig hardForkConsensusConfigPerEra)
      (InPairs.lift
         proxySingle
         (Requiring . go)
         (translateConsensusState hardForkEraTranslation))
  where
    go :: TranslateEraConsensusState blk blk'
       -> SingleEraConsensusConfig blk
       -> SingleEraConsensusConfig blk'
       -> Align PastConsensusState CurrentConsensusState blk blk'
    go f (SingleEraConsensusConfig cfg) (SingleEraConsensusConfig cfg') =
        Align $ \(CurrentConsensusState cur) -> (
            PastConsensusState cur 0
          , CurrentConsensusState (translateConsensusStateWith f cfg cfg' undefined cur)
          )

injectValidationErr :: Injection SingleEraValidationErr xs blk
                    -> ValidationErr (BlockProtocol blk)
                    -> HardForkValidationErr xs
injectValidationErr inj =
      HardForkValidationErrFromEra
    . OneEraValidationErr
    . unK
    . apFn inj
    . SingleEraValidationErr

injectProof :: Injection SingleEraIsLeader xs blk
            -> (:.:) Maybe SingleEraIsLeader blk
            -> Maybe (OneEraIsLeader xs)
injectProof inj (Comp pf) = (OneEraIsLeader . unK . apFn inj) <$> pf

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving via LiftTelescope PastConsensusState CurrentConsensusState xs
         instance CanHardFork xs => Eq (HardForkConsensusState xs)

deriving via LiftTelescope PastConsensusState CurrentConsensusState xs
         instance CanHardFork xs => Show (HardForkConsensusState xs)

deriving via LiftNamedTelescope "HardForkConsensusState" PastConsensusState CurrentConsensusState xs
         instance CanHardFork xs => NoUnexpectedThunks (HardForkConsensusState xs)

deriving stock    instance CanHardFork xs => Eq                 (HardForkValidationErr xs)
deriving stock    instance CanHardFork xs => Show               (HardForkValidationErr xs)
deriving anyclass instance CanHardFork xs => NoUnexpectedThunks (HardForkValidationErr xs)

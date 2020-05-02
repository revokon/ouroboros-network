{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger (
    distribTopLevelConfig
  ) where

import           Control.Monad.Except
import           Data.Function (on)
import           Data.Functor.Product
import           Data.SOP
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Current
                     (CurrentLedgerState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Current as Current
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.State
                     (LedgerState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.State as LedgerState
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Config
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.SOP
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  IsLedger
-------------------------------------------------------------------------------}

data HardForkLedgerError xs =
    -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)

    -- | We tried to apply a block from the wrong era
  | HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoUnexpectedThunks)

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerCfg (LedgerState (HardForkBlock xs)) = HardForkLedgerConfig xs
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  applyChainTick cfgs slot (HardForkLedgerState ledgerState) =
      fmap HardForkLedgerState $ Telescope.sequence $
        LedgerState.lift cfgs (At slot) aux ledgerState
    where
      aux :: SingleEraBlock blk
          => SingleEraLedgerConfig blk
          -> CurrentLedgerState blk
          -> (Ticked :.: CurrentLedgerState) blk
      aux (SingleEraLedgerConfig cfg) = Comp . applyChainTick cfg slot

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance CanHardFork xs
      => ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs) where

  applyLedgerBlock HardForkLedgerConfig{..}
                   (HardForkBlock (OneEraBlock block))
                   (Ticked slot (HardForkLedgerState st)) =
      case Match.matchTelescope block (hmap (Comp . Ticked slot) st) of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
          throwError $ HardForkLedgerErrorWrongEra . MismatchEraInfo $
                         Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
          fmap HardForkLedgerState $ hsequence' $
            hczipWith3 proxySingle apply cfgs injections matched
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

  reapplyLedgerBlock HardForkLedgerConfig{..}
                     (HardForkBlock (OneEraBlock block))
                     (Ticked slot (HardForkLedgerState st)) =
      case Match.matchTelescope block (hmap (Comp . Ticked slot) st) of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyLedgerBlock: can't be from other era"
        Right matched ->
          HardForkLedgerState $
            hczipWith proxySingle reapply cfgs matched
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

  ledgerTipPoint =
        castPoint
      . oneEraLedgerStateTip
      . LedgerState.oneEraLedgerState

apply :: SingleEraBlock blk
      => SingleEraLedgerConfig blk
      -> Injection SingleEraLedgerError xs blk
      -> Product I (Ticked :.: CurrentLedgerState) blk
      -> (Except (HardForkLedgerError xs) :.: CurrentLedgerState) blk
apply (SingleEraLedgerConfig cfg) injectErr (Pair (I block) (Comp st)) = Comp $
    withExcept (injectLedgerError injectErr) $
      Current.liftTickedM (applyLedgerBlock cfg block) st

reapply :: SingleEraBlock blk
        => SingleEraLedgerConfig blk
        -> Product I (Ticked :.: CurrentLedgerState) blk
        -> CurrentLedgerState blk
reapply (SingleEraLedgerConfig cfg) (Pair (I block) (Comp st)) =
    Current.liftTicked (reapplyLedgerBlock cfg block) st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance CanHardFork xs => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance CanHardFork xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkTransitions = LedgerState.transitions
  hardForkShape _     = History.Shape . exactlyFromNP
                      . hcmap proxySingle aux
                      . getPerEraLedgerConfig
                      . hardForkLedgerConfigPerEra
    where
      aux :: forall blk. SingleEraBlock blk
          => SingleEraLedgerConfig blk -> K History.EraParams blk
      aux cfg = K $ singleEraParams (Proxy @blk) (getSingleEraLedgerConfig cfg)

{-------------------------------------------------------------------------------
  HeaderValidation
-------------------------------------------------------------------------------}
instance CanHardFork xs => HasAnnTip (HardForkBlock xs) where
  type TipInfo (HardForkBlock xs) = OneEraTipInfo xs

  getTipInfo =
        OneEraTipInfo
      . hcmap proxySingle (SingleEraTipInfo . getTipInfo)
      . getOneEraHeader
      . getHardForkHeader

  tipInfoHash _ =
        OneEraHash
      . hcmap proxySingle aux
      . getOneEraTipInfo
    where
      aux :: forall blk. SingleEraBlock blk
          => SingleEraTipInfo blk -> SingleEraHash blk
      aux = SingleEraHash . tipInfoHash (Proxy @blk) . getSingleEraTipInfo

data HardForkEnvelopeErr xs =
    -- | Validation error from one of the eras
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkEnvelopeErrWrongEra (MismatchEraInfo xs)
  deriving (Eq, Show, Generic, NoUnexpectedThunks)

instance CanHardFork xs => ValidateEnvelope (HardForkBlock xs) where
  type OtherHeaderEnvelopeError (HardForkBlock xs) = HardForkEnvelopeErr xs

  expectedFirstBlockNo _ =
      case isNonEmpty (Proxy @xs) of
        IsNonEmpty p -> expectedFirstBlockNo p

  minimumPossibleSlotNo _ =
      case isNonEmpty (Proxy @xs) of
        IsNonEmpty p -> minimumPossibleSlotNo p

  -- TODO: If the block is from a different era as the current tip, we just
  -- expect @succ b@. This may not be sufficient: if we ever transition /to/
  -- an era with EBBs, this is not correct.
  expectedNextBlockNo _ (OneEraTipInfo oldTip) (OneEraTipInfo newBlock) b =
      case Match.matchNS oldTip newBlock of
        Right matched  -> hcollapse $ hcmap proxySingle aux matched
        Left _mismatch -> succ b
    where
      aux :: forall blk. SingleEraBlock blk
          => Product SingleEraTipInfo SingleEraTipInfo blk
          -> K BlockNo blk
      aux (Pair (SingleEraTipInfo old) (SingleEraTipInfo new)) = K $
          expectedNextBlockNo (Proxy @blk) old new b

  -- TODO: If the block is from a different era as the current tip, we just
  -- expect @succ s@. This may not be sufficient: if we ever transition /to/
  -- an era with EBBs, this is not correct.
  minimumNextSlotNo _ (OneEraTipInfo oldTip) (OneEraTipInfo newBlock) s =
      case Match.matchNS oldTip newBlock of
        Right matched  -> hcollapse $ hcmap proxySingle aux matched
        Left _mismatch -> succ s
    where
      aux :: forall blk. SingleEraBlock blk
          => Product SingleEraTipInfo SingleEraTipInfo blk
          -> K SlotNo blk
      aux (Pair (SingleEraTipInfo old) (SingleEraTipInfo new)) = K $
          minimumNextSlotNo (Proxy @blk) old new s

  checkPrevHash _ =
      go (getCheckEraTransition hardForkEraTransitionCheck) `on` getOneEraHash
    where
      -- This is a pretty straight-forward aligning of two NS, except we allow
      -- the current tip to be /one/ era before the next block; in this case
      -- the 'hardForkEraTransitionCheck' will do the check for us.
      go :: All SingleEraBlock xs'
         => InPairs CheckTransition xs'
         -> NS SingleEraHash xs' -> NS SingleEraHash xs' -> Bool
      go _            (Z h) (Z h')     = aux h h'
      go (PCons _ fs) (S h) (S h')     = go fs h h'
      go (PCons f _)  (Z h) (S (Z h')) = checkTransitionWith f
                                           (getSingleEraHash h)
                                           (BlockHash $ getSingleEraHash h')
      go _            _     _          = False

      aux :: forall blk. SingleEraBlock blk
          => SingleEraHash blk -> SingleEraHash blk -> Bool
      aux = checkPrevHash (Proxy @blk) `on` getSingleEraHash



  additionalEnvelopeChecks tlc =
                           \(Ticked slot (OneEraLedgerView view))
                            (HardForkHeader (OneEraHeader hdr)) ->
      case Match.matchNS hdr (hmap (Comp . Ticked slot) view) of
        Left mismatch ->
          throwError $
            HardForkEnvelopeErrWrongEra . MismatchEraInfo $
              Match.bihcmap proxySingle singleEraInfo ledgerViewInfo mismatch
        Right matched ->
          hcollapse $ hczipWith3 proxySingle aux cfgs injections matched
    where
      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig tlc

      aux :: forall blk. SingleEraBlock blk
          => TopLevelConfig blk
          -> Injection SingleEraEnvelopeErr xs blk
          -> Product Header (Ticked :.: SingleEraLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ()) blk
      aux cfg injErr (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks cfg (getSingleEraLedgerView <$> view) hdr
        where
          injErr' :: OtherHeaderEnvelopeError blk -> HardForkEnvelopeErr xs
          injErr' = HardForkEnvelopeErrFromEra
                  . OneEraEnvelopeErr
                  . unK . apFn injErr
                  . SingleEraEnvelopeErr

distribTopLevelConfig :: SListI xs
                      => TopLevelConfig (HardForkBlock xs)
                      -> NP TopLevelConfig xs
distribTopLevelConfig TopLevelConfig{..} =
    hzipWith3
      (\(SingleEraConsensusConfig cfgConsensus)
        (SingleEraLedgerConfig    cfgLedger)
        cfgBlock -> TopLevelConfig cfgConsensus cfgLedger cfgBlock)
      (getPerEraConsensusConfig $ hardForkConsensusConfigPerEra configConsensus)
      (getPerEraLedgerConfig    $ hardForkLedgerConfigPerEra    configLedger)
      (getPerEraBlockConfig     $ hardForkBlockConfigPerEra     configBlock)

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => LedgerSupportsProtocol (HardForkBlock xs) where
  protocolLedgerView HardForkLedgerConfig{..} =
        OneEraLedgerView
      . hczipWith proxySingle aux cfgs
      . getOneEraLedgerState
      . LedgerState.oneEraLedgerState
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

      aux :: SingleEraBlock blk
          => SingleEraLedgerConfig blk
          -> LedgerState blk -> SingleEraLedgerView blk
      aux (SingleEraLedgerConfig cfg) = SingleEraLedgerView
                                      . protocolLedgerView cfg

  -- TODO: This is completely wrong (#2082)
  ledgerViewForecastAt cfgs (HardForkLedgerState ledgerState) slot =
      unComp .  fmap OneEraLedgerView $
        sequence_NS' . Telescope.tip $
         LedgerState.lift cfgs slot aux ledgerState
    where
      aux :: SingleEraBlock blk
          => SingleEraLedgerConfig blk
          -> CurrentLedgerState blk
          -> ((Maybe :.: Forecast) :.: SingleEraLedgerView) blk
      aux (SingleEraLedgerConfig cfg) cur = Comp $ Comp $
          fmap SingleEraLedgerView <$>
            ledgerViewForecastAt
              cfg
              (currentLedgerState cur)
              slot

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => (Ticked :.: CurrentLedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk. SingleEraBlock blk
               => (Ticked :.: SingleEraLedgerView) blk -> LedgerEraInfo blk
ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: Injection SingleEraLedgerError xs blk
                  -> LedgerError blk
                  -> HardForkLedgerError xs
injectLedgerError inj =
      HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . unK
    . apFn inj
    . SingleEraLedgerError

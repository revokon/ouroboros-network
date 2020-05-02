{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Ledger.State (LedgerState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.State as LedgerState
module Ouroboros.Consensus.HardFork.Combinator.Ledger.State (
    LedgerState(..)
  , oneEraLedgerState
  , recover
  , lift
  , transitions
  ) where

import           Control.Monad (guard)
import           Data.Functor.Product
import           Data.SOP

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Current
                     (CurrentLedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Past
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.DerivingVia
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), Telescope)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

newtype instance LedgerState (HardForkBlock xs) = HardForkLedgerState {
      getHardForkLedgerState :: Telescope PastLedgerState CurrentLedgerState xs
    }

oneEraLedgerState :: CanHardFork xs
                  => LedgerState (HardForkBlock xs) -> OneEraLedgerState xs
oneEraLedgerState = OneEraLedgerState
                  . hmap currentLedgerState
                  . Telescope.tip
                  . getHardForkLedgerState

{-------------------------------------------------------------------------------
  Serialisation support
-------------------------------------------------------------------------------}

-- | Recover 'LedgerState' from partial information
--
-- The primary goal of this is to make sure that for the /current/ ledger
-- state we really only store the underlying 'LedgerState'. It is not
-- strictly essential that this is possible but it helps with the unary
-- hardfork case, and it may in general help with binary compatibility.
recover :: CanHardFork xs
        => HardForkLedgerConfig xs
        -> Telescope PastLedgerState LedgerState xs
        -> LedgerState (HardForkBlock xs)
recover HardForkLedgerConfig{..} =
      HardForkLedgerState
    . Telescope.bihmap (\(Pair _ past) -> past) recoverCurrent
    . Telescope.withHistory
    . Telescope.bihmap withEndOfEra id
  where
    withEndOfEra :: PastLedgerState blk
                 -> Product (K History.Bound) PastLedgerState blk
    withEndOfEra past = Pair (K $ pastLedgerEnd past) past

    recoverCurrent :: Product (K [History.Bound]) LedgerState blk
                   -> CurrentLedgerState blk
    recoverCurrent (Pair (K past) st) = flip CurrentLedgerState st $
        case past of
          prev:_distantPast -> prev
          []                -> hardForkLedgerConfigStart

{-------------------------------------------------------------------------------
  Chain tick
-------------------------------------------------------------------------------}

-- | Lift operation on 'LedgerState' to the hard fork ledger state
--
-- The slot number will be used to transition to the next era if appropriate;
-- the supplied function is then executed in that new era.
lift :: forall g xs. CanHardFork xs
     => HardForkLedgerConfig xs
     -> WithOrigin SlotNo
     -> (forall blk. SingleEraBlock blk
                  => SingleEraLedgerConfig blk
                  -> CurrentLedgerState blk -> g blk)
     -> Telescope PastLedgerState CurrentLedgerState xs
     -> Telescope PastLedgerState g xs
lift HardForkLedgerConfig{..} mSlot g =
    Telescope.extend
      (InPairs.requiring cfgs $
         InPairs.lift proxySingle (Requiring . next) translate)
      (hcmap proxySingle (fn . g) cfgs)
  where
    cfgs      = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    translate = translateLedgerState hardForkEraTranslation

    -- If 'slot' is in the next era, translate current one to the next one
    next :: SingleEraBlock blk
         => TranslateEraLedgerState blk blk'
         -> SingleEraLedgerConfig blk
         -> SingleEraLedgerConfig blk'
         -> Extend PastLedgerState CurrentLedgerState blk blk'
    next f (SingleEraLedgerConfig cfg)
           (SingleEraLedgerConfig cfg') = Extend $ \cur -> do
        e    <- singleEraTransition cfg (currentLedgerState cur)
        past <- inNextEra cfg cur e
        return (past, CurrentLedgerState {
            currentLedgerStart = pastLedgerEnd past
          , currentLedgerState = translateLedgerStateWith f cfg cfg' $
                                   currentLedgerState cur
          })

    -- Is 'slot' in the next era?
    inNextEra :: forall blk. SingleEraBlock blk
              => LedgerConfig blk
              -> CurrentLedgerState blk  -- Current era
              -> EpochNo                 -- Start of the next
              -> Maybe (PastLedgerState blk)
    inNextEra cfg cur curEnd = do
        -- Upper bound is exclusive
        slot <- withOriginToMaybe mSlot
        guard $ slot >= History.boundSlot curEndBound
        return $ PastLedgerState (currentLedgerStart cur) curEndBound
      where
        curEndBound :: History.Bound
        curEndBound = History.mkUpperBound
                        (singleEraParams (Proxy @blk) cfg)
                        (currentLedgerStart cur)
                        curEnd

{-------------------------------------------------------------------------------
  Find all transitions

  TODO: If we make 'hardForkSummary' the primitive function in
  'HasHardForkHistory', ideally this should not be necessary anymore: the
  summary is trivially derivable from the ledger state. This would then
  also obsolete the need for caching.
-------------------------------------------------------------------------------}

transitions :: forall xs. CanHardFork xs
            => HardForkLedgerConfig xs
            -> LedgerState (HardForkBlock xs) -> History.Transitions xs
transitions HardForkLedgerConfig{..} (HardForkLedgerState st) =
    case isNonEmpty (Proxy @xs) of
      IsNonEmpty _ ->
        History.Transitions $
          shiftTransitions (getPerEraLedgerConfig cfg) $
            allTransitions (getPerEraLedgerConfig cfg) st
  where
    cfg = hardForkLedgerConfigPerEra

-- | Find transition points in all eras
--
-- This associates each transition with the era it transitions /from/.
-- See also 'shiftTransitions'.
allTransitions :: CanHardFork                                  xs
               => NP SingleEraLedgerConfig                     xs
               -> Telescope PastLedgerState CurrentLedgerState xs
               -> AtMost                                       xs EpochNo
allTransitions cfgs st =
    Telescope.toAtMost $
      Telescope.bihap
        (hpure (fn past))
        (hcmap proxySingle (fn . cur . getSingleEraLedgerConfig) cfgs)
        st
  where
    past :: PastLedgerState blk -> K EpochNo blk
    past = K . pastLedgerTransition

    cur :: SingleEraBlock blk
        => LedgerConfig blk -> CurrentLedgerState blk -> K (Maybe EpochNo) blk
    cur cfg = K . singleEraTransition cfg . currentLedgerState

-- | Associate transitions with the era they transition /to/
--
-- 'allTransitions' associates transitions with the era in which they occur,
-- but the hard fork history infrastructure expects them to be associated with
-- the era that they transition /to/. 'shiftTransitions' implements this
-- shift of perspective, and also verifies that the final era cannot have
-- a transition.
shiftTransitions :: NP f (x ': xs) -- Just as an index
                 -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
shiftTransitions = go
  where
    go :: NP f (x ': xs)
       -> AtMost (x ': xs) EpochNo -> AtMost xs EpochNo
    go _                  AtMostNil                = AtMostNil
    go (_ :* cs@(_ :* _)) (AtMostCons t ts)        = AtMostCons t (go cs ts)
    go (_ :* Nil)         (AtMostCons _ AtMostNil) = error invalidTransition

    invalidTransition :: String
    invalidTransition = "Unexpected transition in final era"

{-------------------------------------------------------------------------------
  Required LedgerState instances
-------------------------------------------------------------------------------}

deriving via LiftTelescope PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => Eq (LedgerState (HardForkBlock xs))

deriving via LiftTelescope PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => Show (LedgerState (HardForkBlock xs))

deriving via LiftNamedTelescope "HardForkLedgerState" PastLedgerState CurrentLedgerState xs
         instance CanHardFork xs => NoUnexpectedThunks (LedgerState (HardForkBlock xs))

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Intended for qualied import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Ledger.Current (CurrentLedgerState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Current as Current
module Ouroboros.Consensus.HardFork.Combinator.Ledger.Current (
    CurrentLedgerState(..)
  , lift
  , liftM
  , liftTicked
  , liftTickedM
  ) where

import           Data.Functor.Identity
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract

{-------------------------------------------------------------------------------
  Current ledger state
-------------------------------------------------------------------------------}

data CurrentLedgerState blk = CurrentLedgerState {
      currentLedgerStart :: !(History.Bound)
    , currentLedgerState :: !(LedgerState blk)
    }
  deriving stock (Generic)

lift :: (LedgerState blk -> LedgerState blk)
     -> CurrentLedgerState blk -> CurrentLedgerState blk
lift f = runIdentity . liftM (Identity . f)

liftM :: Functor f
      => (LedgerState blk -> f (LedgerState blk))
      -> CurrentLedgerState blk -> f (CurrentLedgerState blk)
liftM f (CurrentLedgerState start cur) =
    CurrentLedgerState start <$> f cur

liftTicked :: (Ticked (LedgerState blk) -> LedgerState blk)
           -> Ticked (CurrentLedgerState blk) -> CurrentLedgerState blk
liftTicked f = runIdentity . liftTickedM (Identity . f)

liftTickedM :: Functor f
            => (Ticked (LedgerState blk) -> f (LedgerState blk))
            -> Ticked (CurrentLedgerState blk) -> f (CurrentLedgerState blk)
liftTickedM f (Ticked slot (CurrentLedgerState start cur)) =
    CurrentLedgerState start <$> f (Ticked slot cur)

{-------------------------------------------------------------------------------
  IsLedger instance
-------------------------------------------------------------------------------}

instance SingleEraBlock blk => IsLedger (CurrentLedgerState blk) where
  type LedgerCfg (CurrentLedgerState blk) = LedgerConfig blk
  type LedgerErr (CurrentLedgerState blk) = LedgerError  blk

  applyChainTick cfg slot CurrentLedgerState{..} = Ticked slot $
      CurrentLedgerState {
          currentLedgerStart = currentLedgerStart
        , currentLedgerState = ticked
        }
    where
      Ticked _slot ticked = applyChainTick cfg slot currentLedgerState

instance SingleEraBlock blk => ApplyBlock (CurrentLedgerState blk) blk where
  applyLedgerBlock cfg blk (Ticked slot CurrentLedgerState{..}) =
      CurrentLedgerState currentLedgerStart <$>
        applyLedgerBlock cfg blk (Ticked slot currentLedgerState)

  reapplyLedgerBlock cfg blk (Ticked slot CurrentLedgerState{..}) =
      CurrentLedgerState currentLedgerStart $
        reapplyLedgerBlock cfg blk (Ticked slot currentLedgerState)

  ledgerTipPoint = ledgerTipPoint . currentLedgerState

{-------------------------------------------------------------------------------
  Derived type class instances
-------------------------------------------------------------------------------}

deriving stock    instance SingleEraBlock blk => Show               (CurrentLedgerState blk)
deriving stock    instance SingleEraBlock blk => Eq                 (CurrentLedgerState blk)
deriving anyclass instance SingleEraBlock blk => NoUnexpectedThunks (CurrentLedgerState blk)

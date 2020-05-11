{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Mempool (
    HardForkApplyTxErr(..)
  , GenTx(..)
  , TxId(..)
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Current
                     (CurrentLedgerState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Current as Current
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.State
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

data HardForkApplyTxErr xs =
    -- | Validation error from one of the eras
    HardForkApplyTxErrFromEra (OneEraApplyTxErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkApplyTxErrWrongEra (MismatchEraInfo xs)
  deriving (Generic)

instance CanHardFork xs => ApplyTx (HardForkBlock xs) where
  newtype GenTx (HardForkBlock xs) = HardForkGenTx {
        getHardForkGenTx :: OneEraGenTx xs
      }
    deriving (NoUnexpectedThunks)

  type ApplyTxErr (HardForkBlock xs) = HardForkApplyTxErr xs

  applyTx   = applyHelper applyTx
  reapplyTx = applyHelper reapplyTx

applyHelper
  :: forall xs. CanHardFork xs
  => (    forall blk. (SingleEraBlock blk, HasCallStack)
       => LedgerConfig blk
       -> GenTx blk
       -> TickedLedgerState blk
       -> Except (ApplyTxErr blk) (TickedLedgerState blk)
     )
  -> LedgerConfig (HardForkBlock xs)
  -> GenTx (HardForkBlock xs)
  -> TickedLedgerState (HardForkBlock xs)
  -> Except (HardForkApplyTxErr xs) (TickedLedgerState (HardForkBlock xs))
applyHelper apply
            HardForkLedgerConfig{..} =
            \(HardForkGenTx (OneEraGenTx tx))
             (Ticked slot (HardForkLedgerState st)) ->
    case Match.matchTelescope tx (hmap (Comp . Ticked slot) st) of
      Left mismatch ->
        throwError $ HardForkApplyTxErrWrongEra . MismatchEraInfo $
          Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
      Right matched ->
        fmap (fmap HardForkLedgerState . Telescope.sequence) $ hsequence' $
          hczipWith3 proxySingle applyCurrent cfgs injections matched
  where
    cfgs :: NP SingleEraLedgerConfig xs
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    applyCurrent
      :: forall blk. SingleEraBlock blk
      => SingleEraLedgerConfig blk
      -> Injection SingleEraApplyTxErr xs blk
      -> Product GenTx (Ticked :.: CurrentLedgerState) blk
      -> (Except (HardForkApplyTxErr xs) :.: (Ticked :.: CurrentLedgerState)) blk
    applyCurrent (SingleEraLedgerConfig cfg) injectErr (Pair tx (Comp st)) = Comp $ fmap Comp $
      withExcept (injectApplyTxErr injectErr) $
        Current.liftTickedM' (apply cfg tx) st

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  newtype TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
        getHardForkGenTxId :: OneEraGenTxId xs
      }
    deriving (Eq, Ord, NoUnexpectedThunks)

  txId = HardForkGenTxId . OneEraGenTxId
       . hcmap proxySingle (SingleEraGenTxId . txId)
       . getOneEraGenTx . getHardForkGenTx

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- TODO copied from Ouroboros.Consensus.HardFork.Combinator.Ledger
ledgerInfo :: forall blk. SingleEraBlock blk
           => (Ticked :.: CurrentLedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectApplyTxErr :: Injection SingleEraApplyTxErr xs blk
                 -> ApplyTxErr blk
                 -> HardForkApplyTxErr xs
injectApplyTxErr inj =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . unK
    . apFn inj
    . SingleEraApplyTxErr

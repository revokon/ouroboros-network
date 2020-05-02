{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Mempool (
    HardForkApplyTxErr(..)
  , GenTx(..)
  , TxId(..)
  ) where

import           Data.SOP
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Mempool.API

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

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

  applyTx   = error "TODO"
  reapplyTx = error "TODO"

instance CanHardFork xs => HasTxId (GenTx (HardForkBlock xs)) where
  newtype TxId (GenTx (HardForkBlock xs)) = HardForkGenTxId {
        getHardForkGenTxId :: OneEraGenTxId xs
      }
    deriving (Eq, Ord, NoUnexpectedThunks)

  txId = HardForkGenTxId . OneEraGenTxId
       . hcmap proxySingle (SingleEraGenTxId . txId)
       . getOneEraGenTx . getHardForkGenTx

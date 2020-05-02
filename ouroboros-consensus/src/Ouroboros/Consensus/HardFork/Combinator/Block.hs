{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.HardFork.Combinator.Block (
    HardForkBlock(..)
    -- * Type family instances
  , BlockConfig(..)
  , CodecConfig(..)
  , Header(..)
  ) where

import           Data.FingerTree.Strict (Measured (..))
import           Data.SOP

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra

{-------------------------------------------------------------------------------
  Hard fork block
-------------------------------------------------------------------------------}

newtype HardForkBlock xs = HardForkBlock {
      getHardForkBlock :: OneEraBlock xs
    }

newtype instance CodecConfig (HardForkBlock xs) = HardForkCodecConfig {
      hardForkCodecConfigPerEra :: PerEraCodecConfig xs
    }

newtype instance BlockConfig (HardForkBlock xs) = HardForkBlockConfig {
      hardForkBlockConfigPerEra :: PerEraBlockConfig xs
    }
  deriving (NoUnexpectedThunks)

instance CanHardFork xs => BlockHasCodecConfig (HardForkBlock xs) where
  getCodecConfig =
        HardForkCodecConfig
      . PerEraCodecConfig
      . hcmap proxySingle getCodecConfig
      . getPerEraBlockConfig
      . hardForkBlockConfigPerEra

instance CanHardFork xs => GetHeader (HardForkBlock xs) where
  newtype Header (HardForkBlock xs) = HardForkHeader {
        getHardForkHeader :: OneEraHeader xs
      }
    deriving (NoUnexpectedThunks)

  getHeader = HardForkHeader . oneEraBlockHeader . getHardForkBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash (HardForkBlock xs) = OneEraHash xs

instance CanHardFork xs => StandardHash (HardForkBlock xs)

instance CanHardFork xs => Measured BlockMeasure (HardForkBlock xs) where
  measure = blockMeasure

instance CanHardFork xs => HasHeader (HardForkBlock xs) where
  blockHash      =            blockHash     . getHeader
  blockPrevHash  = castHash . blockPrevHash . getHeader
  blockSlot      =            blockSlot     . getHeader
  blockNo        =            blockNo       . getHeader
  blockInvariant = const True

instance CanHardFork xs => HasHeader (Header (HardForkBlock xs)) where
  blockHash      =            blockHash     . getHardForkHeader
  blockPrevHash  = castHash . blockPrevHash . getHardForkHeader
  blockSlot      =            blockSlot     . getHardForkHeader
  blockNo        =            blockNo       . getHardForkHeader
  blockInvariant = const True

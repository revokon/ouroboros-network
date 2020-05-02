{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info (
    SingleEraInfo(..)
  ) where

import           Data.Text (Text)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks (..))

-- | Information about an era (mostly for type errors)
data SingleEraInfo blk = SingleEraInfo {
      singleEraName :: !Text
    }
  deriving stock    (Generic, Eq, Show)
  deriving anyclass (NoUnexpectedThunks)

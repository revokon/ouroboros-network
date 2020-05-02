{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs(..), Requiring(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
module Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (
    -- * InPairs
    InPairs(..)
  , lift
    -- * Requiring
  , Requiring(..)
  , requiring
  ) where

import           Data.SOP

{-------------------------------------------------------------------------------
  InPairs
-------------------------------------------------------------------------------}

-- | We have an @f x y@ for each pair @(x, y)@ of successive list elements
data InPairs (f :: k -> k -> *) (xs :: [k]) where
  PNil  :: InPairs f '[x]
  PCons :: f x y -> InPairs f (y ': zs) -> InPairs f (x ': y ': zs)

lift :: forall proxy c f g xs. All c xs
     => proxy c
     -> (forall x y. (c x, c y) => f x y -> g x y)
     -> InPairs f xs -> InPairs g xs
lift _ f = go
  where
    go :: All c xs' => InPairs f xs' -> InPairs g xs'
    go PNil         = PNil
    go (PCons x xs) = PCons (f x) (go xs)

{-------------------------------------------------------------------------------
  Requiring
-------------------------------------------------------------------------------}

data Requiring h f x y = Requiring {
      provideWith :: h x -> h y -> f x y
    }

requiring :: NP h xs -> InPairs (Requiring h f) xs -> InPairs f xs
requiring = flip go
  where
    go :: InPairs (Requiring h f) xs -> NP h xs -> InPairs f xs
    go PNil         _              = PNil
    go (PCons f fs) (x :* y :* zs) = PCons (provideWith f x y) (go fs (y :* zs))

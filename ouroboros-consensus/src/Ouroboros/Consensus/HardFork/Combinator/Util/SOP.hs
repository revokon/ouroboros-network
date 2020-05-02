{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Util.SOP (
    sequence_NS'
  , IsNonEmpty(..)
  , NonEmpty(..)
  ) where

import           Data.SOP

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..),
                     allNoUnexpectedThunks)

-- | Version of 'sequence_NS' that requires only 'Functor'
--
-- The version in the library requires 'Applicative', which is unnecessary.
sequence_NS' :: forall xs f g. Functor f
             => NS (f :.: g) xs -> f (NS g xs)
sequence_NS' = go
  where
    go :: NS (f :.: g) xs' -> f (NS g xs')
    go (Z (Comp fx)) = Z <$> fx
    go (S r)         = S <$> go r

{-------------------------------------------------------------------------------
  Type-level non-empty lists
-------------------------------------------------------------------------------}

data IsNonEmpty :: [*] -> * where
  IsNonEmpty :: Proxy x -> IsNonEmpty (x ': xs)

class NonEmpty xs where
  isNonEmpty :: proxy xs -> IsNonEmpty xs

instance NonEmpty (x ': xs) where
  isNonEmpty _ = IsNonEmpty (Proxy @x)

{-------------------------------------------------------------------------------
  Orphans

  TODO: These won't be orphans anymore once we introduce a strict SOP layer.
-------------------------------------------------------------------------------}

instance All (Compose NoUnexpectedThunks f) xs
      => NoUnexpectedThunks (NS (f :: k -> *) (xs :: [k])) where
  showTypeOf _ = "NS"
  whnfNoUnexpectedThunks ctxt = \case
      Z l -> noUnexpectedThunks ("Z" : ctxt) l
      S r -> noUnexpectedThunks ("S" : ctxt) r

instance All (Compose NoUnexpectedThunks f) xs
      => NoUnexpectedThunks (NP (f :: k -> *) (xs :: [k])) where
  showTypeOf _ = "NP"
  whnfNoUnexpectedThunks ctxt = \case
      Nil     -> return NoUnexpectedThunks
      x :* xs -> allNoUnexpectedThunks [
                     noUnexpectedThunks ("fst" : ctxt) x
                   , noUnexpectedThunks ("snd" : ctxt) xs
                   ]

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
module Ouroboros.Consensus.HardFork.Combinator.Util.Telescope (
    -- * Telescope
    Telescope(..)
  , sequence
    -- ** Utilities
  , tip
  , toAtMost
  , fromTZ
    -- ** Bifunctor analogues of SOP functions
  , bihap
  , bihctraverse'
  , bihtraverse'
  , bihsequence'
  , bihctraverse_
  , bihtraverse_
  , bihmap
  , bihcmap
  , hfirst
  , hsecond
    -- * SimpleTelescope
  , SimpleTelescope(..)
    -- * Telescope specific operations
  , Extend(..)
  , extend
  , Align(..)
  , align
  , AlignCtxt(..)
  , alignCtxt
  , UpdateTip(..)
  , RestoreTip(..)
  , retract
  , PassForward(..)
  , passForward
  , withHistory
  ) where

import           Prelude hiding (sequence, zipWith)

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Functor.Product
import           Data.Kind
import           Data.SOP
import           Data.SOP.Constraint
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..),
                     allNoUnexpectedThunks)
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..))

{-------------------------------------------------------------------------------
  Telescope
-------------------------------------------------------------------------------}

data Telescope (g :: k -> Type) (f :: k -> Type) (xs :: [k]) where
  TZ :: f x ->                     Telescope g f (x ': xs)
  TS :: g x -> Telescope g f xs -> Telescope g f (x ': xs)

{-------------------------------------------------------------------------------
  SOP class instances for 'Telescope'
-------------------------------------------------------------------------------}

type instance Prod    (Telescope g)   = NP
type instance SListIN (Telescope g)   = SListI
type instance AllN    (Telescope g) c = All c

instance HAp (Telescope g) where
  hap = flip go
    where
      -- We could define this in terms of 'bihap' but we lack 'SListI'
      go :: Telescope g f xs -> NP (f -.-> f') xs -> Telescope g f' xs
      go (TZ fx)   (f :* _)  = TZ (apFn f fx)
      go (TS gx t) (_ :* fs) = TS gx (go t fs)

instance HTraverse_ (Telescope g) where
  hctraverse_ p = bihctraverse_ p (\_ -> pure ())
  htraverse_    = bihtraverse_    (\_ -> pure ())

instance HSequence (Telescope g) where
  hsequence'    = bihsequence' . bihmap (Comp . pure) id
  hctraverse' p = bihctraverse' p pure
  htraverse'    = bihtraverse'    pure

-- | Specialization of 'hsequence'' with weaker constraints
-- ('Functor' rather than 'Applicative')
sequence :: forall m g f xs. Functor m
         => Telescope g (m :.: f) xs -> m (Telescope g f xs)
sequence = go
  where
    go :: Telescope g (m :.: f) xs' -> m (Telescope g f xs')
    go (TZ (Comp fx)) = TZ <$> fx
    go (TS gx t)      = TS gx <$> go t

{-------------------------------------------------------------------------------
  Bifunctor analogues of class methods
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hap'
bihap :: NP (g -.-> g') xs
      -> NP (f -.-> f') xs
      -> Telescope g f xs -> Telescope g' f' xs
bihap = \gs fs t -> go t gs fs
  where
    go :: Telescope g f xs
       -> NP (g -.-> g') xs
       -> NP (f -.-> f') xs
       -> Telescope g' f' xs
    go (TZ fx)   _         (f :* _)  = TZ (apFn f fx)
    go (TS gx t) (g :* gs) (_ :* fs) = TS (apFn g gx) (go t gs fs)

-- | Bifunctor analogue of 'hctraverse''
bihctraverse' :: forall proxy c m g g' f f' xs. (All c xs, Applicative m)
              => proxy c
              -> (forall x. c x => g x -> m (g' x))
              -> (forall x. c x => f x -> m (f' x))
              -> Telescope g f xs -> m (Telescope g' f' xs)
bihctraverse' _ g f = go
  where
    go :: All c xs' => Telescope g f xs' -> m (Telescope g' f' xs')
    go (TZ fx)   = TZ <$> f fx
    go (TS gx t) = TS <$> g gx <*> go t

-- | Bifunctor analogue of 'htraverse''
bihtraverse' :: (SListI xs, Applicative m)
             => (forall x. g x -> m (g' x))
             -> (forall x. f x -> m (f' x))
             -> Telescope g f xs -> m (Telescope g' f' xs)
bihtraverse' = bihctraverse' (Proxy @Top)

-- | Bifunctor analogue of 'hsequence''
bihsequence' :: forall m g f xs. (SListI xs, Applicative m)
             => Telescope (m :.: g) (m :.: f) xs -> m (Telescope g f xs)
bihsequence' = bihtraverse' unComp unComp

-- | Bifunctor analogue of 'hctraverse_'
bihctraverse_ :: forall proxy c xs m g f. (All c xs, Applicative m)
              => proxy c
              -> (forall x. c x => g x -> m ())
              -> (forall x. c x => f x -> m ())
              -> Telescope g f xs -> m ()
bihctraverse_ _ g f = go
  where
    go :: All c xs' => Telescope g f xs' -> m ()
    go (TZ fx)   = f fx
    go (TS gx t) = g gx *> go t

bihtraverse_ :: (SListI xs, Applicative m)
             => (forall x. g x -> m ())
             -> (forall x. f x -> m ())
             -> Telescope g f xs -> m ()
bihtraverse_ = bihctraverse_ (Proxy @Top)

{-------------------------------------------------------------------------------
  Bifunctor analogues of derived functions
-------------------------------------------------------------------------------}

-- | Bifunctor analogue of 'hmap'
bihmap :: SListI xs
       => (forall x. g x -> g' x)
       -> (forall x. f x -> f' x)
       -> Telescope g f xs -> Telescope g' f' xs
bihmap = bihcmap (Proxy @Top)

-- | Bifunctor analogue of 'hcmap'
bihcmap :: All c xs
        => proxy c
        -> (forall x. c x => g x -> g' x)
        -> (forall x. c x => f x -> f' x)
        -> Telescope g f xs -> Telescope g' f' xs
bihcmap p g f = bihap (hcpure p (fn g)) (hcpure p (fn f))

-- | Higher order equivalent of 'Data.Bifunctor.first'
hfirst :: SListI xs
       => (forall x. g x -> g' x) -> Telescope g f xs -> Telescope g' f xs
hfirst f = bihmap f id

-- | Higher order equivalent of 'Data.Bifunctor.second'
hsecond :: SListI xs
        => (forall x. f x -> f' x) -> Telescope g f xs -> Telescope g f' xs
hsecond = bihmap id

{-------------------------------------------------------------------------------
  Simple telescope

  Primarily useful as a sanity check of our bifunctor generalizations.
-------------------------------------------------------------------------------}

-- | 'Telescope' with both functors set to the same @f@
newtype SimpleTelescope f xs = SimpleTelescope {
      getSimpleTelescope :: Telescope f f xs
    }

{-------------------------------------------------------------------------------
  SOP class instances for 'SimpleTelescope'
-------------------------------------------------------------------------------}

type instance Prod    SimpleTelescope   = NP
type instance SListIN SimpleTelescope   = SListI
type instance AllN    SimpleTelescope c = All c

instance HAp SimpleTelescope where
  hap fs = SimpleTelescope . bihap fs fs . getSimpleTelescope

instance HTraverse_ SimpleTelescope where
  hctraverse_ p f = bihctraverse_ p f f . getSimpleTelescope
  htraverse_    f = bihtraverse_    f f . getSimpleTelescope

instance HSequence SimpleTelescope where
  hsequence'      = fmap SimpleTelescope . bihsequence'        . getSimpleTelescope
  hctraverse' p f = fmap SimpleTelescope . bihctraverse' p f f . getSimpleTelescope
  htraverse'    f = fmap SimpleTelescope . bihtraverse'    f f . getSimpleTelescope

{-------------------------------------------------------------------------------
  Utilities
-------------------------------------------------------------------------------}

tip :: Telescope g f xs -> NS f xs
tip (TZ   l) = Z l
tip (TS _ r) = S (tip r)

toAtMost :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
toAtMost = go
  where
    go :: Telescope (K a) (K (Maybe a)) xs -> AtMost xs a
    go (TZ (K ma))  = maybe AtMostNil atMostOne ma
    go (TS (K a) t) = AtMostCons a (go t)

fromTZ :: Telescope g f '[x] -> f x
fromTZ (TZ fx)  = fx
fromTZ (TS _ t) = case t of {}

{-------------------------------------------------------------------------------
  Extending the telescope
-------------------------------------------------------------------------------}

data Extend g f x y = Extend { extendWith :: f x -> Maybe (g x, f y) }

-- | Generalization of 'hap' that optionally extends the telescope.
--
-- The appropriate function from the 'NP' is applied at the tip of the telescope
-- after it has been extended as far as possible.
extend :: InPairs (Extend g f) xs
       -> NP (f -.-> f') xs -> Telescope g f xs -> Telescope g f' xs
extend = flip . go
  where
    go :: InPairs (Extend g f) xs
       -> Telescope g f xs -> NP (f -.-> f') xs -> Telescope g f' xs
    go PNil         (TZ fx)   (f :* _)  = TZ (apFn f fx)
    go (PCons g gs) (TZ fx)   (f :* fs) = case extendWith g fx of
                                            Nothing       -> TZ (apFn f fx)
                                            Just (gx, fy) -> TS gx $ go gs (TZ fy) fs
    go (PCons _ gs) (TS gx t) (_ :* fs) = TS gx $ go gs t fs
    go PNil         (TS _  t) _         = case t of {}

_extendExamplePairs :: InPairs (Extend I I) '[(), Bool, Int]
_extendExamplePairs =
      PCons (Extend $ \(I ()) -> Just (I (), I False))
    $ PCons (Extend $ \(I b)  -> guard b >> return (I b, I 0))
    $ PNil

-- | Example use of 'extend'
--
-- This evaluates to
--
-- > TZ (I ())
-- > TS (I ()) (TZ (I True))
-- > TS (I ()) (TS (I True) (TZ (I 1)))
-- > TS (I ()) (TS (I True) (TZ (I 2)))
-- > TS (I ()) (TS (I True) (TZ (I 3)))
_extendExample :: [Telescope I I '[(), Bool, Int]]
_extendExample =
      take 5
    $ iterate (extend _extendExamplePairs
                      (hcpure (Proxy @Enum) (fn (I . succ . unI))))
    $ TZ (I ())

{-------------------------------------------------------------------------------
  Aligning
-------------------------------------------------------------------------------}

data Align g f x y = Align { alignWith :: f x -> (g x, f y) }

-- | Generalization of 'hap' that extends the telescope when required.
--
-- Precondition: the 'Telescope' cannot be ahead of the 'NS'.
align :: HasCallStack
      => InPairs (Align g f) xs
      -> NS (f -.-> f') xs -> Telescope g f xs -> Telescope g f' xs
align = flip . go
  where
    go :: InPairs (Align g f) xs
       -> Telescope g f xs -> NS (f -.-> f') xs -> Telescope g f' xs
    go _            (TZ fx)   (Z f) = TZ (apFn f fx)
    go (PCons _ as) (TS gx t) (S f) = TS gx $ go as t f
    go (PCons a as) (TZ fx)   (S f) = let (gx, fy) = alignWith a fx
                                      in TS gx $ go as (TZ fy) f
    go _            (TS _ _)  (Z _) = error "align: precondition violated"
    go PNil         _         (S f) = case f of {}

_alignExamplePairs :: InPairs (Align I I) '[(), Bool, Int]
_alignExamplePairs =
      PCons (Align $ \(I ()) -> (I (), I False))
    $ PCons (Align $ \(I b)  -> (I b,  I 0))
    $ PNil

-- | Align examples
--
-- These evaluate to
--
-- > TZ (I ())
-- > TS (I ()) (TZ (I True))
-- > TS (I ()) (TS (I False) (TZ (I 1)))
--
-- respectively.
_alignExample1, _alignExample2, _alignExample3 :: Telescope I I '[(), Bool, Int]
_alignExample1 = align _alignExamplePairs       (Z (fn (I . id   . unI)))   (TZ (I ()))
_alignExample2 = align _alignExamplePairs    (S (Z (fn (I . not  . unI))))  (TZ (I ()))
_alignExample3 = align _alignExamplePairs (S (S (Z (fn (I . succ . unI))))) (TZ (I ()))

{-------------------------------------------------------------------------------
  alignReq
-------------------------------------------------------------------------------}

data AlignCtxt h g f x y = AlignCtxt {
      alignWithCtxt :: h y -> f x -> (g x, f y)
    }

-- | Variation on 'align' that provides more context
--
-- 'align' requires that the telescope can be extended without context; this
-- makes it possible to extend the telescope as far as required to line it
-- up with the 'NS'. By contrast, 'alignRequiring' can only extend the
-- telescope by one segment, /but/ because of this the alignment function
-- can be told what it is aligning with.
--
-- Preconditions:
--
-- PRE-1: The 'Telescope' cannot be ahead of the 'NS' (just like 'align')
-- PRE-2: The 'NS' cannot be ahead of the 'Telescope' by more than one segment.
alignCtxt :: InPairs (AlignCtxt h g f) xs
          -> NS h xs -> Telescope g f xs -> Telescope g (Product h f) xs
alignCtxt = flip . go
  where
    go :: InPairs (AlignCtxt h g f) xs
       -> Telescope g f xs -> NS h xs -> Telescope g (Product h f) xs
    go _            (TZ fx)   (Z hx)     = TZ (Pair hx fx)
    go (PCons _ as) (TS gx t) (S hx)     = TS gx $ go as t hx
    go (PCons a _)  (TZ fx)   (S (Z hy)) = let (gx, fy) = alignWithCtxt a hy fx
                                           in TS gx (TZ (Pair hy fy))
    go _            (TS _ _)  (Z _)      = error "alignRequiring: PRE-1 violated"
    go _            _         (S (S _))  = error "alignRequiring: PRE-2 violated"

{-------------------------------------------------------------------------------
  Retraction
-------------------------------------------------------------------------------}

newtype UpdateTip    f x = UpdateTip  { updateTipWith  :: f x -> Maybe (f x) }
newtype RestoreTip g f x = RestoreTip { restoreTipWith :: g x -> Maybe (f x) }

retract :: NP (UpdateTip f) xs
        -> NP (RestoreTip g f) xs
        -> Telescope g f xs -> Maybe (Telescope g f xs)
retract = \upd re t -> go t upd re
  where
    go :: Telescope g f xs
       -> NP (UpdateTip f) xs
       -> NP (RestoreTip g f) xs
       -> Maybe (Telescope g f xs)
    go (TZ fx)   (f :* _)  _         = TZ <$> updateTipWith f fx
    go (TS gx t) (f :* fs) (g :* gs) = (TS gx <$> go t fs gs)
                                   <|> (do restored <- restoreTipWith g gx
                                           TZ <$> updateTipWith f restored)

{-------------------------------------------------------------------------------
  PassForward
-------------------------------------------------------------------------------}

data PassForward h g x y = PassForward {
      passForwardWith :: g x -> h x -> h y
    }

-- | Pass a value along the telescope
--
-- The value at the start of each segment is computed from the value at the
-- start of /previous/ segment and that segment itself.
--
-- >         g x   g y
-- >        /---\ /---\ /--
-- > h x --/    \/    \/
passForward :: h x                                 -- ^ Initial value
            -> InPairs (PassForward h g) (x ': xs) -- ^ Compute next value
            -> Telescope g f (x ': xs)
            -> Telescope (Product h g) (Product h f) (x ': xs)
passForward = flip . go
  where
    go :: h x
       -> Telescope g f (x ': xs)
       -> InPairs (PassForward h g) (x ': xs)
       -> Telescope (Product h g) (Product h f) (x ': xs)
    go _  (TS _  t) PNil         = case t of {}
    go hx (TZ fx)   _            = TZ (Pair hx fx)
    go hx (TS gx t) (PCons f fs) = TS (Pair hx gx) $ go hy t fs
      where
        hy = passForwardWith f gx hx

{-------------------------------------------------------------------------------
  Foo
-------------------------------------------------------------------------------}

-- | Calculate history (new to old) at each segment
--
-- Given a telescope with segments and values
--
--     a0   a1   a2
-- > |----|----|----|
-- >  g x  g y  g z   f tip
--
-- compute
--
--     []   [a0]  [a1,a0]  [a2,a1,a0]
-- > |----|-----|--------|
-- >  g x   g y     g z      f tip
--
-- NOTE: A generalization of this function for type dependent values would
-- be possible, but significantly trickier, as we'd have to describe at the
-- type level how the type of history develops.
withHistory :: Telescope (Product (K  a ) g)                  f  xs
            -> Telescope (Product (K [a]) g) (Product (K [a]) f) xs
withHistory = go []
  where
    go :: [a]
       -> Telescope (Product (K a) g) f xs
       -> Telescope (Product (K [a]) g) (Product (K [a]) f) xs
    go acc (TZ fx)                = TZ (Pair (K acc) fx)
    go acc (TS (Pair (K a) gx) t) = TS (Pair (K acc) gx) $ go (a : acc) t

{-------------------------------------------------------------------------------
  Standard type class instances
-------------------------------------------------------------------------------}

deriving stock instance ( All (Compose Eq g) xs
                        , All (Compose Eq f) xs
                        ) => Eq (Telescope g f xs)

deriving stock instance ( All (Compose Eq  g) xs
                        , All (Compose Ord g) xs
                        , All (Compose Eq  f) xs
                        , All (Compose Ord f) xs
                        ) => Ord (Telescope g f xs)

deriving stock instance ( All (Compose Show g) xs
                        , All (Compose Show f) xs
                        ) => Show (Telescope g f xs)

instance ( All (Compose NoUnexpectedThunks g) xs
         , All (Compose NoUnexpectedThunks f) xs
         ) => NoUnexpectedThunks (Telescope g f xs) where
  showTypeOf _ = "Telescope"
  whnfNoUnexpectedThunks ctxt = \case
      TZ f   -> noUnexpectedThunks ("TZ" : ctxt) f
      TS g t -> allNoUnexpectedThunks [
                    noUnexpectedThunks ("g" : "TS" : ctxt) g
                  , noUnexpectedThunks ("t" : "TS" : ctxt) t
                  ]

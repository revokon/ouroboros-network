{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Storage.ChainDB.API (
    -- * Main ChainDB API
    ChainDB(..)
  , getCurrentTip
  , getTipBlockNo
    -- * Adding a block
  , AddBlockPromise(..)
  , addBlockWaitWrittenToDisk
  , addBlock
  , addBlock_
    -- * Useful utilities
  , getBlock
  , streamBlocks
  , newBlockReader
    -- * Serialised block/header with its point
  , WithPoint(..)
  , getSerialisedBlockWithPoint
  , getSerialisedHeaderWithPoint
  , getPoint
    -- * BlockComponent
  , BlockComponent(..)
    -- * Support for tests
  , toChain
  , fromChain
    -- * Iterator API
  , StreamFrom(..)
  , StreamTo(..)
  , Iterator(..)
  , IteratorResult(..)
  , traverseIterator
  , UnknownRange(..)
  , validBounds
  , streamAll
    -- * Ledger Cursor API
  , LedgerCursor(..)
  , LedgerCursorFailure(..)
  , getPastLedger
    -- * Invalid block reason
  , InvalidBlockReason(..)
    -- * Readers
  , Reader(..)
  , traverseReader
    -- * Recovery
  , ChainDbFailure(..)
  , BlockRef(..)
  , IsEBB(..)
    -- * Exceptions
  , ChainDbError(..)
  ) where

import qualified Codec.CBOR.Read as CBOR
import           Control.Exception (Exception (..))
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import           Data.Typeable
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (ChainUpdate, MaxSlotNo,
                     Serialised (..))
import qualified Ouroboros.Network.Block as Network

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (WithFingerprint)

import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API.Types (FsError, sameFsError)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB

-- Support for tests
import           Ouroboros.Network.MockChain.Chain (Chain (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain

-- | The chain database
--
-- The chain database provides a unified interface on top of:
--
-- * The ImmutableDB, storing the part of the chain that can't roll back.
-- * The VolatileDB, storing the blocks near the tip of the chain, possibly in
--   multiple competing forks.
-- * The LedgerDB, storing snapshots of the ledger state for blocks in the
--   ImmutableDB (and in-memory snapshots for the rest).
--
-- In addition to providing a unifying interface on top of these disparate
-- components, the main responsibilities that the ChainDB itself has are:
--
-- * Chain selection (on initialization and whenever a block is added)
-- * Trigger full recovery whenever we detect disk failure in any component
-- * Provide iterators across fixed fragments of the current chain
-- * Provide readers that track the status of the current chain
--
-- The ChainDB instantiates all the various type parameters of these databases
-- to conform to the unified interface we provide here.
data ChainDB m blk = ChainDB {
      -- | Add a block to the heap of blocks
      --
      -- We do /not/ assume that the block is valid (under the legder rules);
      -- it is the responsibility of the Chain DB itself to only select chains
      -- that are valid.
      --
      -- Conversely, the caller cannot assume that the new block will be added
      -- to the current chain; even if the block is valid, it will not become
      -- part of the chain if there are other chains available that are
      -- preferred by the consensus algorithm (typically, longer chains).
      --
      -- This function typically returns immediately, yielding a
      -- 'AddBlockPromise' which can be used to wait for the result. You can
      -- use 'addBlock' to add the block synchronously.
      --
      -- NOTE: back pressure can be applied when overloaded.
      addBlockAsync      :: blk -> m (AddBlockPromise m blk)

      -- | Get the current chain fragment
      --
      -- Suppose the current chain is
      --
      -- > a -> b -> c -> d -> e -> ff
      --
      -- and suppose @k = 2@; this means that the most distant fork we can
      -- switch to is something like
      --
      -- > a -> b -> c -> d -> e' -> f'
      --
      -- The fragment we return will be @[e, f]@, anchored at @d@. In other
      -- words, the length of the fragment will under normal circumstances
      -- be exactly @k@ blocks long. It may be shorter if
      --
      -- * We are near genesis
      --   The anchor will be the genesis point
      --   (which does not correspond to an actual block)
      --
      -- * The volatile DB suffered some data loss
      --   Typically (but not necessarily) the immutable DB will not be empty
      --   and the anchor will be pointing to the tip of the immutable DB.
      --
      -- POSTCONDITION: The Chain DB will be able to switch to any fork starting
      -- from the anchor of the returned fragment or any subsequent block
      -- (provided the new fork is at least of the same length as the old).
      --
      -- NOTE: A direct consequence of this guarantee is that the anchor of the
      -- fragment will move as the chain grows.
    , getCurrentChain    :: STM m (AnchoredFragment (Header blk))

      -- | Get current ledger
    , getCurrentLedger   :: STM m (ExtLedgerState blk)

      -- | Get block at the tip of the chain, if one exists
      --
      -- Returns 'Nothing' if the database is empty.
    , getTipBlock        :: m (Maybe blk)

      -- | Get header at the tip of the chain
      --
      -- NOTE: Calling 'getTipHeader' is cheaper than 'getTipBlock' and then
      -- extracting the header: most of the time the header at the tip is
      -- actually in memory, whereas the block never is.
      --
      -- Returns 'Nothing' if the database is empty.
    , getTipHeader       :: m (Maybe (Header blk))

      -- | Get point of the tip of the chain
      --
      -- Will return 'genesisPoint' if the database is empty; if the
      -- current chain fragment is empty due to data loss in the volatile DB,
      -- 'getTipPoint' will return the tip of the immutable DB.
    , getTipPoint        :: STM m (Point blk)

      -- | Get the given component(s) of the block at the specified point. If
      -- there is no block at the given point, 'Nothing' is returned.
    , getBlockComponent  :: forall b. BlockComponent (ChainDB m blk) b
                         -> RealPoint blk -> m (Maybe b)

      -- | Return membership check function for recent blocks
      --
      -- This check is only reliable for blocks up to @k@ away from the tip.
      -- For blocks older than that the results should be regarded as
      -- non-deterministic.
    , getIsFetched       :: STM m (Point blk -> Bool)

      -- | Return a function that tells whether a block is known to be valid
      -- or invalid.
      --
      -- The function will return:
      --
      -- * @Just True@: for blocks in the volatile DB that have been validated
      --   and were found to be valid. All blocks in the current chain
      --   fragment (i.e., 'getCurrentChain') are valid.
      --
      -- * @Just False@: for blocks in the volatile DB that have been
      --   validated and were found to be invalid.
      --
      -- * @Nothing@: for blocks not or no longer in the volatile DB, whether
      --   they are valid or not, including blocks in the immutable DB. Also
      --   for blocks in the volatile DB that haven't been validated (yet),
      --   e.g., because they are disconnected from the current chain or they
      --   are part of a shorter fork.
    , getIsValid         :: STM m (RealPoint blk -> Maybe Bool)

      -- | Get the highest slot number stored in the ChainDB.
      --
      -- Note that the corresponding block doesn't have to be part of the
      -- current chain, it could be part of some fork, or even be a
      -- disconnected block.
    , getMaxSlotNo       :: STM m MaxSlotNo

      -- | Stream blocks
      --
      -- Streaming is not restricted to the current fork, but there must be an
      -- unbroken path from the starting point to the end point /at the time
      -- of initialization/ of the iterator. Once the iterator has been
      -- initialized, it will not be affected by subsequent calls to
      -- 'addBlock'. To track the current chain, use a 'Reader' instead.
      --
      -- Streaming blocks older than @k@ is permitted, but only when they are
      -- part of the current fork (at the time of initialization). Streaming a
      -- fork that forks off more than @k@ blocks in the past is not permitted
      -- and an 'UnknownRange' error will be returned in that case.
      --
      -- The iterator /does/ have a limited lifetime, however. The chain DB
      -- internally partitions the chain into an " immutable " part and a
      -- " volatile " part, moving blocks from the volatile DB to the immutable
      -- DB when they become more than @k@ deep into the chain. When a block
      -- with slot number @n@ is added to the immutble DB, a time delay @t@
      -- kicks in; after that time delay expires, all blocks older than @n@ may
      -- be removed from the volatile DB, /including any blocks that happen to
      -- live on other forks/ (since those forks must now, by definition, be too
      -- distant). This time delay @t@ also provides a worst-case bound for the
      -- lifetime of the iterator: if the iterator traverses a chain that
      -- forks off from our current chain at the tip of the immutable DB,
      -- then the first block on that fork will become unavailable as soon as
      -- another block is pushed to the current chain and the subsequent
      -- time delay expires.
      --
      -- Note: although blocks are moved from the volatile DB to the immutable
      -- DB after they have become @k@ deep into the chain, due to data
      -- corruption the suffix of the chain in the volatile DB might be
      -- shorter than @k@. The immutable DB /always/ determines the maximum
      -- rollback, which may therefore be shorter than @k@ under such
      -- circumstances. In addition, streaming blocks which aren't on the
      -- current fork is permitted, but the oldest volatile block must fit on
      -- to the tip of the immutable DB.
      --
      -- When the given bounds are nonsensical, an 'InvalidIteratorRange' is
      -- thrown.
      --
      -- When the given bounds are not part of the chain DB, an 'UnknownRange'
      -- error is returned.
      --
      -- To stream all blocks from the current chain, use 'streamAll', as it
      -- correctly handles an empty ChainDB.
    , stream
        :: forall b.
           ResourceRegistry m
        -> BlockComponent (ChainDB m blk) b
        -> StreamFrom blk -> StreamTo blk
        -> m (Either (UnknownRange blk) (Iterator m blk b))

      -- | Chain reader
      --
      -- A chain reader is an iterator that tracks the state of the /current/
      -- chain: calling @next@ on the iterator will either give you the next
      -- block header, or (if we have switched to a fork) the instruction to
      -- rollback.
      --
      -- The tracking iterator starts at genesis (see also 'trackForward').
      --
      -- This is intended for use by chain consumers to /reliably/ follow a
      -- chain, desipite the chain being volatile.
      --
      -- Examples of users:
      -- * The server side of the chain sync mini-protocol for the
      --   node-to-node protocol using headers and the block size.
      -- * The server side of the chain sync mini-protocol for the
      --   node-to-client protocol using blocks.
      --
    , newReader
        :: forall b.
           ResourceRegistry m
        -> BlockComponent (ChainDB m blk) b
        -> m (Reader m blk b)

      -- | Get a ledger cursor that is focused on the ledger corresponding to
      -- the tip of the current chain (see 'getCurrentLedger').
    , newLedgerCursor   :: m (LedgerCursor m blk)

      -- | Function to check whether a block is known to be invalid.
      --
      -- Blocks unknown to the ChainDB will result in 'False'.
      --
      -- If the hash corresponds to a block that is known to be invalid, but
      -- is now older than @k@, this function may return 'False'.
      --
      -- Whenever a new invalid block is added, the @Fingerprint@ will be
      -- changed. This is useful when \"watching\" this function in a
      -- transaction.
      --
      -- Note that when invalid blocks are garbage collected and thus no
      -- longer detected by this function, the 'Fingerprint' doesn't have to
      -- change, since the function will not detect new invalid blocks.
    , getIsInvalidBlock :: STM m (WithFingerprint (HeaderHash blk -> Maybe (InvalidBlockReason blk)))

      -- | Close the ChainDB
      --
      -- Idempotent.
      --
      -- Should only be called on shutdown.
    , closeDB            :: m ()

      -- | Return 'True' when the database is open.
      --
      -- 'False' when the database is closed.
    , isOpen             :: STM m Bool
    }

getCurrentTip :: (Monad (STM m), HasHeader (Header blk))
              => ChainDB m blk -> STM m (Network.Tip blk)
getCurrentTip = fmap (AF.anchorToTip . AF.headAnchor) . getCurrentChain

getTipBlockNo :: (Monad (STM m), HasHeader (Header blk))
              => ChainDB m blk -> STM m (WithOrigin BlockNo)
getTipBlockNo = fmap Network.getTipBlockNo . getCurrentTip

instance DB (ChainDB m blk) where
  -- Returning a block or header requires parsing. In case of failure, a
  -- 'ChainDbFailure' exception is thrown
  type DBBlock      (ChainDB m blk) = m blk
  type DBHeader     (ChainDB m blk) = m (Header blk)
  type DBHeaderHash (ChainDB m blk) = HeaderHash blk

{-------------------------------------------------------------------------------
  Adding a block
-------------------------------------------------------------------------------}

data AddBlockPromise m blk = AddBlockPromise
    { blockWrittenToDisk :: STM m Bool
      -- ^ Use this 'STM' transaction to wait until the block has been written
      -- to disk.
      --
      -- Returns 'True' when the block was written to disk or 'False' when it
      -- was ignored, e.g., because it was older than @k@.
      --
      -- If the 'STM' transaction has returned 'True' then 'getIsFetched' will
      -- return 'True' for the added block.
      --
      -- NOTE: Even when the result is 'False', 'getIsFetched' might still
      -- return 'True', e.g., the block was older than @k@, but it has been
      -- downloaded and stored on disk before.
    , blockProcessed     :: STM m (Point blk)
      -- ^ Use this 'STM' transaction to wait until the block has been
      -- processed: the block has been written to disk and chain selection has
      -- been performed for the block, /unless/ the block is from the future.
      --
      -- The ChainDB's tip after chain selection is returned. When this tip
      -- doesn't match the added block, it doesn't necessarily mean the block
      -- wasn't adopted. We might have adopted a longer chain of which the
      -- added block is a part, but not the tip.
      --
      -- NOTE: When the block is from the future, chain selection for the
      -- block won't be performed until the block is no longer in the future,
      -- which might take some time. For that reason, this transaction will
      -- not wait for chain selection of a block from the future. It will
      -- return the current tip of the ChainDB after writing the block to
      -- disk.
    }

-- | Add a block synchronously: wait until the block has been written to disk
-- (see 'blockWrittenToDisk').
addBlockWaitWrittenToDisk :: IOLike m => ChainDB m blk -> blk -> m Bool
addBlockWaitWrittenToDisk chainDB blk = do
    promise <- addBlockAsync chainDB blk
    atomically $ blockWrittenToDisk promise

-- | Add a block synchronously: wait until the block has been processed (see
-- 'blockProcessed'). The new tip of the ChainDB is returned.
addBlock :: IOLike m => ChainDB m blk -> blk -> m (Point blk)
addBlock chainDB blk = do
    promise <- addBlockAsync chainDB blk
    atomically $ blockProcessed promise

-- | Add a block synchronously. Variant of 'addBlock' that doesn't return the
-- new tip of the ChainDB.
addBlock_ :: IOLike m => ChainDB m blk -> blk -> m ()
addBlock_  = void .: addBlock

{-------------------------------------------------------------------------------
  Useful utilities
-------------------------------------------------------------------------------}

-- These are all variants of ChainDB methods instantiated to a specific
-- BlockComponent.

-- | Get block at the specified point (if it exists).
getBlock :: Monad m => ChainDB m blk -> RealPoint blk -> m (Maybe blk)
getBlock ChainDB { getBlockComponent } pt =
    sequence =<< getBlockComponent GetBlock pt

streamBlocks
  :: Monad m
  => ChainDB m blk
  -> ResourceRegistry m
  -> StreamFrom blk
  -> StreamTo blk
  -> m (Either (UnknownRange blk) (Iterator m blk blk))
streamBlocks ChainDB { stream } rr from to =
    fmap (traverseIterator id) <$> stream rr GetBlock from to

newBlockReader
  :: Monad m
  => ChainDB m blk
  -> ResourceRegistry m
  -> m (Reader m blk blk)
newBlockReader ChainDB { newReader } rr =
    traverseReader id <$> newReader rr GetBlock

{-------------------------------------------------------------------------------
  Serialised block/header with its point
-------------------------------------------------------------------------------}

-- | A @b@ together with its 'Point'.
--
-- The 'Point' is needed because we often need to know the hash, slot, or
-- point itself of the block or header in question, and we don't want to
-- deserialise the block to obtain it.
data WithPoint blk b = WithPoint
   { withoutPoint :: !b
   , point        :: !(Point blk)
   }

type instance HeaderHash (WithPoint blk b) = HeaderHash blk
instance StandardHash blk => StandardHash (WithPoint blk b)

getPoint :: BlockComponent (ChainDB m blk) (Point blk)
getPoint = BlockPoint <$> GetSlot <*> GetHash

getSerialisedBlockWithPoint
  :: BlockComponent (ChainDB m blk) (WithPoint blk (Serialised blk))
getSerialisedBlockWithPoint =
    WithPoint <$> (Serialised <$> GetRawBlock) <*> getPoint

getSerialisedHeader
  :: forall m blk. ReconstructNestedCtxt Header blk
  => BlockComponent (ChainDB m blk) (SerialisedHeader blk)
getSerialisedHeader =
    mkSerialisedHeader
      <$> GetNestedCtxt
      <*> GetBlockSize
      <*> GetRawHeader
  where
    mkSerialisedHeader
      :: ShortByteString
      -> SizeInBytes
      -> Lazy.ByteString
      -> SerialisedHeader blk
    mkSerialisedHeader prefix blockSize rawHdr =
        serialisedHeaderFromPair (
            reconstructNestedCtxt (Proxy @(Header blk)) prefix blockSize
          , rawHdr
          )

getSerialisedHeaderWithPoint
  :: ReconstructNestedCtxt Header blk
  => BlockComponent (ChainDB m blk) (WithPoint blk (SerialisedHeader blk))
getSerialisedHeaderWithPoint =
    WithPoint <$> getSerialisedHeader <*> getPoint

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

toChain :: forall m blk. (HasCallStack, IOLike m, HasHeader blk)
        => ChainDB m blk -> m (Chain blk)
toChain chainDB = withRegistry $ \registry ->
    streamAll chainDB registry >>= maybe (return Genesis) (go Genesis)
  where
    go :: Chain blk -> Iterator m blk blk -> m (Chain blk)
    go chain it = do
      next <- iteratorNext it
      case next of
        IteratorResult blk  -> go (Chain.addBlock blk chain) it
        IteratorExhausted   -> return chain
        IteratorBlockGCed _ ->
          error "block on the current chain was garbage-collected"

fromChain :: forall m blk. IOLike m
          => m (ChainDB m blk)
          -> Chain blk
          -> m (ChainDB m blk)
fromChain openDB chain = do
    chainDB <- openDB
    mapM_ (addBlock_ chainDB) $ Chain.toOldestFirst chain
    return chainDB

{-------------------------------------------------------------------------------
  Iterator API
-------------------------------------------------------------------------------}

-- | The lower bound for a ChainDB iterator.
--
-- Hint: use @'StreamFromExclusive' 'genesisPoint'@ to start streaming from
-- Genesis.
data StreamFrom blk =
    StreamFromInclusive !(RealPoint blk)
  | StreamFromExclusive !(Point     blk)
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

data StreamTo blk =
    StreamToInclusive !(RealPoint blk)
  | StreamToExclusive !(RealPoint blk)
  deriving (Show, Eq, Generic, NoUnexpectedThunks)

data Iterator m blk b = Iterator {
      iteratorNext  :: m (IteratorResult blk b)
    , iteratorClose :: m ()
      -- ^ When 'fmap'-ing or 'traverse'-ing (or using 'traverseIterator') an
      -- 'Iterator', the resulting iterator will still refer to and use the
      -- original one. This means that when either of them is closed, both
      -- will be closed in practice.
    } deriving (Functor, Foldable, Traversable)

-- | Variant of 'traverse' instantiated to @'Iterator' m blk@ that executes
-- the monadic function when calling 'iteratorNext'.
traverseIterator
  :: Monad m
  => (b -> m b')
  -> Iterator m blk b
  -> Iterator m blk b'
traverseIterator f it = it {
      iteratorNext = iteratorNext it >>= traverse f
    }

data IteratorResult blk b =
    IteratorExhausted
  | IteratorResult b
  | IteratorBlockGCed (RealPoint blk)
    -- ^ The block that was supposed to be streamed was garbage-collected from
    -- the VolatileDB, but not added to the ImmutableDB.
    --
    -- This will only happen when streaming very old forks very slowly.
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq   blk, Eq   b, StandardHash blk)
               => Eq   (IteratorResult blk b)
deriving instance (Show blk, Show b, StandardHash blk)
               => Show (IteratorResult blk b)

data UnknownRange blk =
    -- | The block at the given point was not found in the ChainDB.
    MissingBlock (RealPoint blk)
    -- | The requested range forks off too far in the past, i.e. it doesn't
    -- fit on the tip of the ImmutableDB.
  | ForkTooOld (StreamFrom blk)
  deriving (Eq, Show)

-- | Check whether the bounds make sense
--
-- An example of bounds that don't make sense:
--
-- > StreamFromExclusive (Point { pointSlot = SlotNo 3 , .. }
-- > StreamToInclusive   (Point { pointSlot = SlotNo 3 , .. }
--
-- FIXME StreamFrom and StreamTo can be refined to not admit origin points
-- in cases where it doesn't make sense.
validBounds :: StreamFrom blk -> StreamTo blk -> Bool
validBounds from to = case from of

  StreamFromExclusive GenesisPoint -> True

  StreamFromInclusive (RealPoint sfrom _) -> case to of
    StreamToInclusive (RealPoint sto _) -> sfrom <= sto
    StreamToExclusive (RealPoint sto _) -> sfrom <  sto

  StreamFromExclusive (BlockPoint sfrom _) -> case to of
    StreamToInclusive (RealPoint sto _) -> sfrom <  sto
    StreamToExclusive (RealPoint sto _) -> sfrom <  sto

-- | Stream all blocks from the current chain.
--
-- To stream all blocks from the current chain from the ChainDB, one would use
-- @'StreamFromExclusive' 'genesisPoint'@ as the lower bound and
-- @'StreamToInclusive' tip@ as the upper bound where @tip@ is retrieved with
-- 'getTipPoint'.
--
-- However, when the ChainDB is empty, @tip@ will be 'genesisPoint' too, in
-- which case the bounds don't make sense. This function correctly handles
-- this case.
--
-- Note that this is not a 'Reader', so the stream will not include blocks
-- that are added to the current chain after starting the stream.
streamAll :: (IOLike m, StandardHash blk)
          => ChainDB m blk
          -> ResourceRegistry m
          -> m (Maybe (Iterator m blk blk))
streamAll chainDB registry = do
    tip <- atomically $ getTipPoint chainDB
    case pointToWithOriginRealPoint tip of
      Origin         -> return Nothing
      NotOrigin tip' -> do
        errIt <- streamBlocks
                   chainDB
                   registry
                   (StreamFromExclusive GenesisPoint)
                   (StreamToInclusive tip')
        case errIt of
          -- TODO this is theoretically possible if the current chain has
          -- changed significantly between getting the tip and asking for the
          -- stream.
          Left  e  -> error (show e)
          Right it -> return $ Just it

{-------------------------------------------------------------------------------
  Ledger cursor API
-------------------------------------------------------------------------------}

-- | A potentially more efficient way to obtain past ledger snapshots.
--
-- NOTE: a 'LedgerCursor' currently allocates no resources that need explicit
-- cleanup, so there is no @ledgerCursorClose@ operation.
data LedgerCursor m blk = LedgerCursor {
      ledgerCursorState :: m (ExtLedgerState blk)
      -- ^ The ledger state the cursor is pointing at.
    , ledgerCursorMove  :: Point blk
                        -> m (Either LedgerCursorFailure (ExtLedgerState blk))
      -- ^ Move the ledger cursor to the given point.
      --
      -- This cannot live in STM, because the ledger DB does not store all
      -- ledger snapshots, and so getting a past ledger DB may involve reading
      -- from disk.
      --
      -- When the cursor could not be moved to the given point, a
      -- 'LedgerCursorFailure' is returned, otherwise the request ledger state
      -- is returned.
    }

-- | Returned in case 'LedgerCursorMove' failed.
data LedgerCursorFailure
  = PointTooOld
    -- ^ The given point corresponds to a block older than @k@.
    --
    -- It might also /not/ be on the chain.
  | PointNotOnChain
    -- ^ The given point is not on the chain.
  deriving (Eq, Show)

-- | Utility function to get the ledger state at the given point.
--
-- Is a combination of 'newLedgerCursor' and 'ledgerCursorMove'.
--
-- See the docstring of 'newLedgerCursor' for more information.
getPastLedger :: Monad m
              => ChainDB m blk -> Point blk -> m (Maybe (ExtLedgerState blk))
getPastLedger chainDB pt = do
    cursor <- newLedgerCursor chainDB
    either (const Nothing) Just <$> ledgerCursorMove cursor pt

{-------------------------------------------------------------------------------
  Invalid block reason
-------------------------------------------------------------------------------}

-- | The reason why a block is invalid.
data InvalidBlockReason blk
  = ValidationError !(ExtValidationError blk)
    -- ^ The ledger found the block to be invalid.
  | InFutureExceedsClockSkew !(RealPoint blk)
    -- ^ The block's slot is in the future, exceeding the allowed clock skew.
    --
    -- Possible causes, order by decreasing likelihood:
    --
    -- 1. Our clock is behind (significantly more likely than the others)
    -- 2. Their clock is ahead
    -- 3. It's intentional, i.e., an attack
  deriving (Eq, Show, Generic)

instance LedgerSupportsProtocol blk
      => NoUnexpectedThunks (InvalidBlockReason blk)

{-------------------------------------------------------------------------------
  Readers
-------------------------------------------------------------------------------}

-- | Reader
--
-- See 'newHeaderReader' for more info.
--
-- The type parameter @a@ will be instantiated with @blk@ or @Header @blk@.
data Reader m blk a = Reader {
      -- | The next chain update (if one exists)
      --
      -- Not in @STM@ because might have to read the blocks or headers from
      -- disk.
      --
      -- We may roll back more than @k@, but only in case of data loss.
      readerInstruction         :: m (Maybe (ChainUpdate blk a))

      -- | Blocking version of 'readerInstruction'
    , readerInstructionBlocking :: m (ChainUpdate blk a)

      -- | Move the reader forward
      --
      -- Must be given a list of points in order of preference; the iterator
      -- will move forward to the first point on the list that is on the current
      -- chain. Returns 'Nothing' if the iterator did not move, or the new point
      -- otherwise.
      --
      -- When successful, the first call to 'readerInstruction' after
      -- 'readerForward' will be a 'RollBack' to the point returned by
      -- 'readerForward'.
      --
      -- Cannot live in @STM@ because the points specified might live in the
      -- immutable DB.
    , readerForward             :: [Point blk] -> m (Maybe (Point blk))

      -- | Close the reader.
      --
      -- Idempotent.
      --
      -- After closing, all other operations on the reader will throw
      -- 'ClosedReaderError'.
    , readerClose               :: m ()
    }
  deriving (Functor)

-- | Variant of 'traverse' instantiated to @'Reader' m blk@ that executes the
-- monadic function when calling 'readerInstruction' and
-- 'readerInstructionBlocking'.
traverseReader
  :: Monad m
  => (b -> m b')
  -> Reader m blk b
  -> Reader m blk b'
traverseReader f rdr = Reader
    { readerInstruction         = readerInstruction         rdr >>= traverse (traverse f)
    , readerInstructionBlocking = readerInstructionBlocking rdr >>= traverse f
    , readerForward             = readerForward             rdr
    , readerClose               = readerClose               rdr
    }

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Reference to a block used in 'ChainDbFailure'.
data BlockRef blk = BlockRef
  { blockRefPoint :: !(Point blk)
  , blockRefIsEBB :: !IsEBB
  } deriving (Eq, Show)

-- | Database failure
--
-- This exception wraps any kind of unexpected problem with the on-disk
-- storage of the chain.
--
-- The various constructors only serve to give more detailed information about
-- what went wrong, in case sysadmins want to investigate the disk failure.
-- The Chain DB itself does not differentiate; all disk failures are treated
-- equal and all trigger the same recovery procedure.
data ChainDbFailure =
    -- | A block in the immutable or volatile DB failed to parse
    forall blk. (Typeable blk, StandardHash blk) =>
      ChainDbParseFailure (BlockRef blk) CBOR.DeserialiseFailure

    -- | When parsing a block from the immutable or volatile DB we got some trailing data
  | forall blk. (Typeable blk, StandardHash blk) =>
      ChainDbTrailingData (BlockRef blk) Lazy.ByteString

    -- | Block missing from the immutable DB
    --
    -- This exception gets thrown when a block that we /know/ should exist in
    -- the DB (for example, because we have its successor) nonetheless was
    -- not found
  | ImmDbMissingBlock (Either EpochNo SlotNo)

    -- | Block missing from the immutable DB
    --
    -- Same as 'ImmDbMissingBlock', but we only know the 'Point' of the block.
  | forall blk. (Typeable blk, StandardHash blk) =>
      ImmDbMissingBlockPoint
        (Point blk)
        (ImmDB.WrongBoundError (HeaderHash blk))
        CallStack

    -- | We requested an iterator that was immediately exhausted
    --
    -- When we ask the immutable DB for an iterator with a particular start
    -- position but no stop position, the resulting iterator cannot be
    -- exhausted immediately: the start position is inclusive, the DB would
    -- throw an exception if the slot number is beyond the last written block,
    -- and the DB does not contain any trailing empty slots.
  | forall blk. (Typeable blk, StandardHash blk) =>
      ImmDbUnexpectedIteratorExhausted (Point blk)

    -- | The immutable DB threw an "unexpected error"
    --
    -- These are errors indicative of a disk failure (as opposed to API misuse)
  | ImmDbFailure ImmDB.UnexpectedError

    -- | Block missing from the volatile DB
    --
    -- This exception gets thrown when a block that we /know/ should exist
    -- in the DB (for example, because its hash exists in the volatile DB's
    -- successor index) nonetheless was not found
  | forall blk. (Typeable blk, StandardHash blk) =>
      VolDbMissingBlock (Proxy blk) (HeaderHash blk)

    -- | A block got corrupted in the volatile DB
    --
    -- This exception gets thrown when, while copying blocks from the volatile
    -- DB to the immutable DB, a block doesn't pass the integrity check
    -- (hash/signature check).
  | forall blk. (Typeable blk, StandardHash blk) =>
      VolDbCorruptBlock (BlockRef blk)

    -- | The volatile DB throw an "unexpected error"
    --
    -- These are errors indicative of a disk failure (as opposed to API misuse)
  | VolDbFailure VolDB.UnexpectedError

    -- | The ledger DB threw a file-system error
  | LgrDbFailure FsError

    -- | Block missing from the chain DB
    --
    -- Thrown when we are not sure in which DB the block /should/ have been.
  | forall blk. (Typeable blk, StandardHash blk) =>
      ChainDbMissingBlock (RealPoint blk)
  deriving (Typeable)

deriving instance Show ChainDbFailure

instance Exception ChainDbFailure where
  displayException = \case
      ChainDbParseFailure {}              -> corruption
      ChainDbTrailingData {}              -> corruption
      ImmDbMissingBlock {}                -> corruption
      ImmDbMissingBlockPoint {}           -> corruption
      ImmDbUnexpectedIteratorExhausted {} -> corruption
      ImmDbFailure e -> case e of
        ImmDB.FileSystemError fse      -> fsError fse
        ImmDB.InvalidFileError {}      -> corruption
        ImmDB.MissingFileError {}      -> corruption
        ImmDB.ChecksumMismatchError {} -> corruption
      VolDbMissingBlock {}                -> corruption
      VolDbCorruptBlock {}                -> corruption
      VolDbFailure e -> case e of
        VolDB.FileSystemError fse -> fsError fse
      LgrDbFailure fse                    -> fsError fse
      ChainDbMissingBlock {}              -> corruption
    where
      corruption =
        "The database got corrupted, full validation will be enabled for the next startup"

      -- The output will be a bit too detailed, but it will be quite clear.
      fsError :: FsError -> String
      fsError = displayException

instance Eq ChainDbFailure where
  ChainDbParseFailure (a1 :: BlockRef blk) b1 == ChainDbParseFailure (a2 :: BlockRef blk') b2 =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2 && b1 == b2
  ChainDbParseFailure {} == _ = False

  ChainDbTrailingData (a1 :: BlockRef blk) b1 == ChainDbTrailingData (a2 :: BlockRef blk') b2 =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2 && b1 == b2
  ChainDbTrailingData {} == _ = False

  ImmDbMissingBlock a1 == ImmDbMissingBlock a2 = a1 == a2
  ImmDbMissingBlock {} == _ = False

  ImmDbMissingBlockPoint (a1 :: Point blk) b1 _cs1 == ImmDbMissingBlockPoint (a2 :: Point blk') b2 _cs2 =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2 && b1 == b2
  ImmDbMissingBlockPoint {} == _ = False

  ImmDbUnexpectedIteratorExhausted (a1 :: Point blk) == ImmDbUnexpectedIteratorExhausted (a2 :: Point blk') =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2
  ImmDbUnexpectedIteratorExhausted {} == _ = False

  ImmDbFailure a1 == ImmDbFailure a2 = ImmDB.sameUnexpectedError a1 a2
  ImmDbFailure {} == _               = False

  VolDbMissingBlock (Proxy :: Proxy blk) a1 == VolDbMissingBlock (Proxy :: Proxy blk') a2 =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2
  VolDbMissingBlock {} == _ = False

  VolDbCorruptBlock (a1 :: BlockRef blk) == VolDbCorruptBlock (a2 :: BlockRef blk') =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2
  VolDbCorruptBlock {} == _ = False

  VolDbFailure a1 == VolDbFailure a2 = VolDB.sameUnexpectedError a1 a2
  VolDbFailure {} == _               = False

  LgrDbFailure a1 == LgrDbFailure a2 = sameFsError a1 a2
  LgrDbFailure {} == _               = False

  ChainDbMissingBlock (a1 :: RealPoint blk) == ChainDbMissingBlock (a2 :: RealPoint blk') =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> a1 == a2
  ChainDbMissingBlock {} == _ = False

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
data ChainDbError =
    -- | The ChainDB is closed.
    --
    -- This will be thrown when performing any operation on the ChainDB except
    -- for 'isOpen' and 'closeDB'. The 'CallStack' of the operation on the
    -- ChainDB is included in the error.
    ClosedDBError CallStack

    -- | The reader is closed.
    --
    -- This will be thrown when performing any operation on a closed readers,
    -- except for 'readerClose'.
  | ClosedReaderError

    -- | When there is no chain/fork that satisfies the bounds passed to
    -- 'streamBlocks'.
    --
    -- * The lower and upper bound are not on the same chain.
    -- * The bounds don't make sense, e.g., the lower bound starts after the
    --   upper bound, or the lower bound starts from genesis, /inclusive/.
  | forall blk. (Typeable blk, StandardHash blk) =>
      InvalidIteratorRange (StreamFrom blk) (StreamTo blk)
  deriving (Typeable)

deriving instance Show ChainDbError

instance Eq ChainDbError where
  ClosedDBError _ == ClosedDBError _ = True
  ClosedDBError _ == _               = False

  ClosedReaderError == ClosedReaderError = True
  ClosedReaderError == _                 = False

  InvalidIteratorRange (fr :: StreamFrom blk) to == InvalidIteratorRange (fr' :: StreamFrom blk') to' =
    case eqT @blk @blk' of
      Nothing   -> False
      Just Refl -> fr == fr' && to == to'
  InvalidIteratorRange _ _ == _ = False

instance Exception ChainDbError where
  displayException = \case
    -- The user should not see the exception below, a fatal exception with
    -- more information about the specific will have been thrown. This
    -- exception will only be thrown if some thread still tries to use the
    -- ChainDB afterwards, which should not happen.
    ClosedDBError {} ->
      "The database was used after it was closed because it encountered an unrecoverable error"

    -- The user won't see the exceptions below, they are not fatal.
    ClosedReaderError {} ->
      "The block/header reader was used after it was closed"
    InvalidIteratorRange {} ->
      "An invalid range of blocks was requested"

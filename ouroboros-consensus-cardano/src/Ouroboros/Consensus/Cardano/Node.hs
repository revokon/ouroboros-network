{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Node (
    CardanoNodeToNodeVersion (..)
  , CardanoNodeToClientVersion (..)
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)))

import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query ()
import           Ouroboros.Consensus.HardFork.Combinator.Mempool ()
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.State

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork ()

-- Only the serialiser for blocks on disk needs to be compatible
--
-- for things sent across the network, we can just introduce a new version and
-- use the wrapper we want
--
-- ledger snapshot format changes anyway


{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

data CardanoNodeToNodeVersion = CardanoNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data CardanoNodeToClientVersion = CardanoNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion CardanoBlock where
  type NodeToNodeVersion   CardanoBlock = CardanoNodeToNodeVersion
  type NodeToClientVersion CardanoBlock = CardanoNodeToClientVersion

  supportedNodeToNodeVersions   _ = CardanoNodeToNodeVersion1
                                  :| []
  supportedNodeToClientVersions _ = CardanoNodeToClientVersion1
                                  :| []

  mostRecentNodeToNodeVersion   _ = CardanoNodeToNodeVersion1
  mostRecentNodeToClientVersion _ = CardanoNodeToClientVersion1

  nodeToNodeProtocolVersion _ CardanoNodeToNodeVersion1 = undefined -- TODO

  nodeToClientProtocolVersion _ CardanoNodeToClientVersion1 = undefined -- TODO

-- () for Byron, but TPraosNodeState TPraosStandardCrypto for Shelley
-- NS?
type instance NodeState CardanoBlock = () -- TODO

instance RunNode CardanoBlock where

  nodeForgeBlock = undefined

  nodeBlockMatchesHeader = undefined
  nodeBlockFetchSize = undefined
  nodeIsEBB = undefined

  nodeImmDbChunkInfo  = undefined
  nodeStartTime       = undefined
  nodeNetworkMagic    = undefined
  nodeProtocolMagicId = undefined

  nodeHashInfo = undefined
  nodeAddHeaderEnvelope = undefined
  nodeExceptionIsFatal = undefined

  nodeInitChainDB = undefined

  nodeMaxBlockSize = undefined
  nodeBlockEncodingOverhead = undefined

  nodeCheckIntegrity = undefined

  nodeTxSize = undefined

  -- Encoders

  nodeEncodeBlockWithInfo = undefined
  nodeEncodeBlock = undefined
  nodeEncodeHeader = undefined
  nodeEncodeWrappedHeader = undefined
  nodeEncodeGenTx = undefined
  nodeEncodeGenTxId = undefined
  nodeEncodeHeaderHash = undefined
  nodeEncodeLedgerState = undefined
  nodeEncodeConsensusState = undefined
  nodeEncodeApplyTxError = undefined
  nodeEncodeAnnTip = undefined
  nodeEncodeQuery = undefined
  nodeEncodeResult = undefined

  -- Decoders

  nodeDecodeBlock = undefined
  nodeDecodeHeader = undefined
  nodeDecodeWrappedHeader = undefined
  nodeDecodeGenTx = undefined
  nodeDecodeGenTxId = undefined
  nodeDecodeHeaderHash = undefined
  nodeDecodeLedgerState = undefined
  nodeDecodeConsensusState = undefined
  nodeDecodeApplyTxError = undefined
  nodeDecodeAnnTip = undefined
  nodeDecodeQuery = undefined
  nodeDecodeResult = undefined

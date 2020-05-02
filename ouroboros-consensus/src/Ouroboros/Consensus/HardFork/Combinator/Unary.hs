{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Witness isomorphism between @b@ and @HardForkBlock '[b]@
module Ouroboros.Consensus.HardFork.Combinator.Unary (
    -- * Projections
    projAnnTip
  , projApplyTxErr
  , projBlock
  , projBlockConfig
  , projChainHash
  , projCodecConfig
  , projConsensusConfig
  , projConsensusState
  , projGenTx
  , projGenTxId
  , projHeader
  , projHeaderHash
  , projInitChainDB
  , projIsLeader
  , projLedgerConfig
  , projLedgerState
  , projLedgerView
  , projQuery
  , projTipInfo
  , projTopLevelConfig
    -- * Injections
  , injAnnTip
  , injApplyTxErr
  , injBlock
  , injConsensusState
  , injEnvelopeErr
  , injGenTx
  , injGenTxId
  , injHashInfo
  , injHeader
  , injHeaderHash
  , injLedgerState
  , injQuery
  ) where

import           Data.SOP
import           Data.Type.Equality
import           Data.Void

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..))

import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import qualified Ouroboros.Consensus.HardFork.Combinator.Ledger.Current as Ledger.Current
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.State
                     (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.Config
import qualified Ouroboros.Consensus.HardFork.Combinator.Protocol.Current as Consensus.Current
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.State
                     (HardForkConsensusState (..))
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Combined
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Projections
-------------------------------------------------------------------------------}

projIsLeader :: IsLeader (BlockProtocol (HardForkBlock '[b]))
             -> IsLeader (BlockProtocol b)
projIsLeader = getSingleEraIsLeader . unZ . getOneEraIsLeader

projGenTx :: GenTx (HardForkBlock '[b]) -> GenTx b
projGenTx = unZ . getOneEraGenTx . getHardForkGenTx

injGenTx :: GenTx b -> GenTx (HardForkBlock '[b])
injGenTx = HardForkGenTx . OneEraGenTx . Z

projGenTxId :: GenTxId (HardForkBlock '[b]) -> GenTxId b
projGenTxId = getSingleEraGenTxId . unZ . getOneEraGenTxId . getHardForkGenTxId

injGenTxId :: GenTxId b -> GenTxId (HardForkBlock '[b])
injGenTxId = HardForkGenTxId . OneEraGenTxId . Z . SingleEraGenTxId

projBlock :: HardForkBlock '[b] -> b
projBlock = unI . unZ . getOneEraBlock . getHardForkBlock

injBlock :: b -> HardForkBlock '[b]
injBlock = HardForkBlock . OneEraBlock . Z . I

projHeaderHash :: HeaderHash (HardForkBlock '[b]) -> HeaderHash b
projHeaderHash = getSingleEraHash . unZ . getOneEraHash

injHeaderHash :: HeaderHash b -> HeaderHash (HardForkBlock '[b])
injHeaderHash = OneEraHash . Z . SingleEraHash

projChainHash :: ChainHash (HardForkBlock '[b]) -> ChainHash b
projChainHash GenesisHash   = GenesisHash
projChainHash (BlockHash h) = BlockHash (projHeaderHash h)

projHeader :: Header (HardForkBlock '[b]) -> Header b
projHeader = unZ . getOneEraHeader . getHardForkHeader

injHeader :: Header b -> Header (HardForkBlock '[b])
injHeader = HardForkHeader . OneEraHeader . Z

projBlockConfig :: BlockConfig (HardForkBlock '[b]) -> BlockConfig b
projBlockConfig = hd . getPerEraBlockConfig . hardForkBlockConfigPerEra

projCodecConfig :: CodecConfig (HardForkBlock '[b]) -> CodecConfig b
projCodecConfig = hd . getPerEraCodecConfig . hardForkCodecConfigPerEra

projLedgerConfig :: LedgerConfig (HardForkBlock '[b]) -> LedgerConfig b
projLedgerConfig =
      getSingleEraLedgerConfig
    . hd
    . getPerEraLedgerConfig
    . hardForkLedgerConfigPerEra

projConsensusConfig :: ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
                    -> ConsensusConfig (BlockProtocol b)
projConsensusConfig =
      getSingleEraConsensusConfig
    . hd
    . getPerEraConsensusConfig
    . hardForkConsensusConfigPerEra

projLedgerState :: LedgerState (HardForkBlock '[b]) -> LedgerState b
projLedgerState =
      Ledger.Current.currentLedgerState
    . Telescope.fromTZ
    . getHardForkLedgerState

injLedgerState :: SystemStart -> LedgerState b -> LedgerState (HardForkBlock '[b])
injLedgerState systemStart =
      HardForkLedgerState
    . Telescope.TZ
    . Ledger.Current.CurrentLedgerState (History.initBound systemStart)

projLedgerView :: proxy b
               -> LedgerView (BlockProtocol (HardForkBlock '[b]))
               -> LedgerView (BlockProtocol b)
projLedgerView _ =
      getSingleEraLedgerView
    . unZ
    . getOneEraLedgerView

projConsensusState :: ConsensusState (BlockProtocol (HardForkBlock '[b]))
                   -> ConsensusState (BlockProtocol b)
projConsensusState =
      Consensus.Current.currentConsensusState
    . Telescope.fromTZ
    . getHardForkConsensusState

injConsensusState :: ConsensusState (BlockProtocol b)
                  -> ConsensusState (BlockProtocol (HardForkBlock '[b]))
injConsensusState =
      HardForkConsensusState
    . Telescope.TZ
    . Consensus.Current.CurrentConsensusState

projTopLevelConfig :: TopLevelConfig (HardForkBlock '[b]) -> TopLevelConfig b
projTopLevelConfig = hd . distribTopLevelConfig

injHashInfo :: HashInfo (HeaderHash b)
            -> HashInfo (HeaderHash (HardForkBlock '[b]))
injHashInfo info = HashInfo {
      hashSize = hashSize info
    , getHash  = injHeaderHash <$> getHash info
    , putHash  = putHash info . projHeaderHash
    }

projInitChainDB :: InitChainDB m (HardForkBlock '[b]) -> InitChainDB m b
projInitChainDB initDB = InitChainDB.InitChainDB {
      InitChainDB.checkEmpty = InitChainDB.checkEmpty initDB
    , InitChainDB.addBlock   = InitChainDB.addBlock initDB . injBlock
    }

projApplyTxErr :: ApplyTxErr (HardForkBlock '[b]) -> ApplyTxErr b
projApplyTxErr (HardForkApplyTxErrFromEra err) =
      getSingleEraApplyTxErr
    . unZ
    . getOneEraApplyTxErr
    $ err
projApplyTxErr (HardForkApplyTxErrWrongEra err) =
      absurd
    . mismatchOneEra
    $ err

injApplyTxErr :: ApplyTxErr b -> ApplyTxErr (HardForkBlock '[b])
injApplyTxErr =
      HardForkApplyTxErrFromEra
    . OneEraApplyTxErr
    . Z
    . SingleEraApplyTxErr

projTipInfo :: TipInfo (HardForkBlock '[b]) -> TipInfo b
projTipInfo =
      getSingleEraTipInfo
    . unZ
    . getOneEraTipInfo

projAnnTip :: AnnTip (HardForkBlock '[b]) -> AnnTip b
projAnnTip (AnnTip s b nfo) = AnnTip s b (projTipInfo nfo)

injAnnTip :: AnnTip b -> AnnTip (HardForkBlock '[b])
injAnnTip (AnnTip s b nfo) =
    AnnTip s b (OneEraTipInfo (Z (SingleEraTipInfo nfo)))

projQuery :: Query (HardForkBlock '[b]) result
          -> (forall result'.
                  (result :~: HardForkQueryResult '[b] result')
               -> Query b result'
               -> a)
          -> a
projQuery (HardForkQuery (QZ qry)) k = k Refl qry
projQuery (HardForkQuery (QS qry)) _ = case qry of {}

injQuery :: Query b result
         -> Query (HardForkBlock '[b]) (HardForkQueryResult '[b] result)
injQuery = HardForkQuery . QZ

injEnvelopeErr :: OtherHeaderEnvelopeError b
               -> OtherHeaderEnvelopeError (HardForkBlock '[b])
injEnvelopeErr = undefined

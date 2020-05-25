{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Witness isomorphism between @b@ and @HardForkBlock '[b]@
module Ouroboros.Consensus.HardFork.Combinator.Unary (
    FromRawHash(..)
    -- * Projections
  , projAnnTip
  , projApplyTxErr
  , projBlock
  , projBlockConfig
  , projChainHash
  , projCodecConfig
  , projConsensusConfig
  , projConsensusState
  , projExtLedgerState
  , projForgeState
  , projGenTx
  , projGenTxId
  , projHeader
  , projHeaderHash
  , projHeaderState
  , projInitChainDB
  , projIsLeader
  , projLedgerConfig
  , projLedgerState
  , projLedgerView
  , projQuery
  , projTipInfo
  , projTopLevelConfig
  , projUpdateForgeState
    -- * Injections
  , injAnnTip
  , injApplyTxErr
  , injBlock
  , injBlockConfig
  , injCodecConfig
  , injConsensusConfig
  , injConsensusState
  , injEnvelopeErr
  , injExtLedgerState
  , injForgeState
  , injGenTx
  , injGenTxId
  , injHashInfo
  , injHeader
  , injHeaderHash
  , injHeaderState
  , injLedgerConfig
  , injLedgerState
  , injProtocolInfo
  , injProtocolClientInfo
  , injQuery
  , injTopLevelConfig
  ) where

import qualified Data.ByteString as Strict
import           Data.SOP.Strict
import           Data.Type.Equality
import           Data.Void

import           Cardano.Slotting.EpochInfo

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Block.Forge
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.Init as InitChainDB
import           Ouroboros.Consensus.Storage.ImmutableDB (HashInfo (..))

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Forge ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
                     (HardForkEraLedgerView (..))
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Combined
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers
import           Ouroboros.Consensus.HardFork.Combinator.State
                     (HardForkState_ (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Addtional requirements
-------------------------------------------------------------------------------}

-- | Construct hash from the raw bytes
--
-- Consensus never needs to go this direction, but the projection
-- functions do. We make this a separate type class so that we can still
-- give a 'RunNode' instance.
class FromRawHash blk where
  fromRawHash :: proxy blk -> Strict.ByteString -> HeaderHash blk

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

projHeaderHash :: forall b. FromRawHash b
               => HeaderHash (HardForkBlock '[b]) -> HeaderHash b
projHeaderHash = fromRawHash (Proxy @b) . getOneEraHash

injHeaderHash :: forall b. SingleEraBlock b
              => HeaderHash b -> HeaderHash (HardForkBlock '[b])
injHeaderHash = OneEraHash . getRawHash (Proxy @b)

projChainHash :: FromRawHash b
              => ChainHash (HardForkBlock '[b]) -> ChainHash b
projChainHash = \case
    GenesisHash -> GenesisHash
    BlockHash h -> BlockHash (projHeaderHash h)

projHeader :: Header (HardForkBlock '[b]) -> Header b
projHeader = unZ . getOneEraHeader . getHardForkHeader

injHeader :: Header b -> Header (HardForkBlock '[b])
injHeader = HardForkHeader . OneEraHeader . Z

projBlockConfig :: BlockConfig (HardForkBlock '[b]) -> BlockConfig b
projBlockConfig = hd . getPerEraBlockConfig . hardForkBlockConfigPerEra

injBlockConfig :: BlockConfig b -> BlockConfig (HardForkBlock '[b])
injBlockConfig = HardForkBlockConfig . PerEraBlockConfig . (:* Nil)

projCodecConfig :: CodecConfig (HardForkBlock '[b]) -> CodecConfig b
projCodecConfig = hd . getPerEraCodecConfig . hardForkCodecConfigPerEra

injCodecConfig :: CodecConfig b -> CodecConfig (HardForkBlock '[b])
injCodecConfig = HardForkCodecConfig . PerEraCodecConfig . (:* Nil)

projLedgerConfig :: forall b. SingleEraBlock b
                 => LedgerConfig (HardForkBlock '[b])
                 -> (EpochInfo Identity, LedgerConfig b)
projLedgerConfig =
      complete
    . getSingleEraLedgerConfig
    . hd
    . getPerEraLedgerConfig
    . hardForkLedgerConfigPerEra
  where
    complete :: PartialLedgerConfig b -> (EpochInfo Identity, LedgerConfig b)
    complete cfg = (ei, completeLedgerConfig (Proxy @b) ei cfg)
      where
        ei :: EpochInfo Identity
        ei = fixedSizeEpochInfo $
               History.eraEpochSize (singleEraParams (Proxy @b) cfg)

-- TODO generalise this function to no longer require this constraint
injLedgerConfig :: PartialLedgerConfig b ~ LedgerConfig b
                => SecurityParam
                -> History.EraParams
                -> LedgerConfig b
                -> LedgerConfig (HardForkBlock '[b])
injLedgerConfig k eraParams cfg = HardForkLedgerConfig {
      hardForkLedgerConfigK      = k
    , hardForkLedgerConfigShape  = History.singletonShape eraParams
    , hardForkLedgerConfigPerEra = PerEraLedgerConfig (SingleEraLedgerConfig cfg :* Nil)
    }

projConsensusConfig :: forall b. SingleEraBlock b
                    => EpochInfo Identity
                    -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
                    -> ConsensusConfig (BlockProtocol b)
projConsensusConfig ei =
      completeConsensusConfig (Proxy @(BlockProtocol b)) ei
    . getSingleEraConsensusConfig
    . hd
    . getPerEraConsensusConfig
    . hardForkConsensusConfigPerEra

injConsensusConfig :: forall b.
                      ( SingleEraBlock b
                        -- TODO generalise this function to no longer require
                        -- this constraint
                      , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                      )
                   => History.EraParams
                   -> ConsensusConfig (BlockProtocol b)
                   -> ConsensusConfig (BlockProtocol (HardForkBlock '[b]))
injConsensusConfig eraParams cfg = HardForkConsensusConfig {
      hardForkConsensusConfigK      = protocolSecurityParam cfg
    , hardForkConsensusConfigShape  = History.singletonShape eraParams
    , hardForkConsensusConfigPerEra = PerEraConsensusConfig (SingleEraConsensusConfig cfg :* Nil)
    }

projLedgerState :: LedgerState (HardForkBlock '[b]) -> LedgerState b
projLedgerState =
      State.currentState
    . Telescope.fromTZ
    . getHardForkState
    . getHardForkLedgerState

injLedgerState :: SystemStart -> LedgerState b -> LedgerState (HardForkBlock '[b])
injLedgerState systemStart =
      HardForkLedgerState
    . HardForkState
    . Telescope.TZ
    . State.Current (History.initBound systemStart)

projLedgerView :: proxy b
               -> LedgerView (BlockProtocol (HardForkBlock '[b]))
               -> LedgerView (BlockProtocol b)
projLedgerView _ =
      hardForkEraLedgerView
    . State.fromTZ

projConsensusState :: ConsensusState (BlockProtocol (HardForkBlock '[b]))
                   -> ConsensusState (BlockProtocol b)
projConsensusState =
      getSingleEraConsensusState
    . State.currentState
    . Telescope.fromTZ
    . getHardForkState

injConsensusState :: SystemStart
                  -> ConsensusState (BlockProtocol b)
                  -> ConsensusState (BlockProtocol (HardForkBlock '[b]))
injConsensusState systemStart =
      HardForkState
    . Telescope.TZ
    . State.Current (History.initBound systemStart)
    . SingleEraConsensusState

projHeaderState :: HeaderState (HardForkBlock '[b])
                -> HeaderState b
projHeaderState HeaderState{..} = HeaderState {
      headerStateConsensus = projConsensusState headerStateConsensus
    , headerStateTips      = projAnnTip <$> headerStateTips
    , headerStateAnchor    = projAnnTip <$> headerStateAnchor
    }

injHeaderState :: SystemStart
               -> HeaderState b
               -> HeaderState (HardForkBlock '[b])
injHeaderState systemStart HeaderState{..} = HeaderState {
      headerStateConsensus = injConsensusState systemStart headerStateConsensus
    , headerStateTips      = injAnnTip <$> headerStateTips
    , headerStateAnchor    = injAnnTip <$> headerStateAnchor
    }

projExtLedgerState :: ExtLedgerState (HardForkBlock '[b]) -> ExtLedgerState b
projExtLedgerState ExtLedgerState{..} = ExtLedgerState {
      ledgerState = projLedgerState ledgerState
    , headerState = projHeaderState headerState
    }

injExtLedgerState :: SystemStart
                  -> ExtLedgerState b
                  -> ExtLedgerState (HardForkBlock '[b])
injExtLedgerState systemStart ExtLedgerState{..} = ExtLedgerState {
      ledgerState = injLedgerState systemStart ledgerState
    , headerState = injHeaderState systemStart headerState
    }

projTopLevelConfig :: forall b. SingleEraBlock b
                   => TopLevelConfig (HardForkBlock '[b]) -> TopLevelConfig b
projTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus  = projConsensusConfig ei configConsensus
    , configLedger     = configLedger'
    , configBlock      = projBlockConfig configBlock
    }
  where
    (ei, configLedger') = projLedgerConfig configLedger

injTopLevelConfig :: forall b.
                     ( SingleEraBlock b
                     , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                     , PartialLedgerConfig b ~ LedgerConfig b
                     )
                  => TopLevelConfig b -> TopLevelConfig (HardForkBlock '[b])
injTopLevelConfig TopLevelConfig{..} = TopLevelConfig{
      configConsensus = injConsensusConfig eraParams configConsensus
    , configLedger    = injLedgerConfig
                          (protocolSecurityParam configConsensus)
                          eraParams
                          configLedger
    , configBlock     = injBlockConfig configBlock
    }
  where
    eraParams = singleEraParams (Proxy @b) configLedger

projForgeState :: proxy b -> ForgeState (HardForkBlock '[b]) -> ForgeState b
projForgeState _ = getSingleEraForgeState . hd . getPerEraForgeState

injForgeState :: proxy b -> ForgeState b -> ForgeState (HardForkBlock '[b])
injForgeState _ = PerEraForgeState . (:* Nil) . SingleEraForgeState

injHashInfo :: (SingleEraBlock b, FromRawHash b)
            => HashInfo (HeaderHash b)
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
injEnvelopeErr =
      HardForkEnvelopeErrFromEra
    . OneEraEnvelopeErr
    . Z
    . SingleEraEnvelopeErr

projUpdateForgeState :: forall b m.
                        Update m (ForgeState (HardForkBlock '[b]))
                     -> Update m (ForgeState b)
projUpdateForgeState = liftUpdate get set
  where
    get :: PerEraForgeState '[b] -> ForgeState b
    get = projForgeState (Proxy @b)

    set :: ForgeState b -> PerEraForgeState '[b] -> PerEraForgeState '[b]
    set = const . injForgeState (Proxy @b)

-- TODO generalise this function to no longer require the equality constraints
injProtocolInfo :: forall b.
                   ( SingleEraBlock b
                   , PartialConsensusConfig (BlockProtocol b) ~ ConsensusConfig (BlockProtocol b)
                   , PartialLedgerConfig b ~ LedgerConfig b
                   )
                => SystemStart
                -> ProtocolInfo b
                -> ProtocolInfo (HardForkBlock '[b])
injProtocolInfo systemStart ProtocolInfo {..} = ProtocolInfo {
      pInfoConfig         = injTopLevelConfig pInfoConfig
    , pInfoInitForgeState = injForgeState
                              (Proxy @b)
                              pInfoInitForgeState
    , pInfoInitLedger     = injExtLedgerState
                              systemStart
                              pInfoInitLedger
    }

injProtocolClientInfo :: ProtocolClientInfo b -> ProtocolClientInfo (HardForkBlock '[b])
injProtocolClientInfo ProtocolClientInfo{..} = ProtocolClientInfo {
      pClientInfoCodecConfig = injCodecConfig pClientInfoCodecConfig
    }
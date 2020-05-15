{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.CanHardFork () where

import           Control.Monad.Reader (runReader)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Hashing as Hashing
import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Translation
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Protocol (PBftByronCrypto)
import           Ouroboros.Consensus.Protocol.PBFT (PBftLedgerView)
import           Ouroboros.Consensus.Protocol.PBFT.State (PBftState)
import qualified Ouroboros.Consensus.Protocol.PBFT.State as PBftState

import           Ouroboros.Consensus.Shelley.Ledger
import qualified Ouroboros.Consensus.Shelley.Ledger.History as History
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Consensus.Shelley.Protocol.State (TPraosState)
import qualified Ouroboros.Consensus.Shelley.Protocol.State as TPraosState

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.Delegation.Certificates as SL
import qualified Shelley.Spec.Ledger.EpochBoundary as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL
import qualified Shelley.Spec.Ledger.Rewards as SL
import qualified Shelley.Spec.Ledger.STS.Prtcl as SL
import qualified Shelley.Spec.Ledger.TxData as SL
import qualified Shelley.Spec.Ledger.UTxO as SL

import           Ouroboros.Consensus.Cardano.Block

{-------------------------------------------------------------------------------
  SingleEraBlock Byron
-------------------------------------------------------------------------------}

instance SingleEraBlock ByronBlock where
  singleEraParams _ = byronEraParams

  singleEraTransition :: LedgerConfig ByronBlock -> LedgerState ByronBlock -> Maybe EpochNo
  singleEraTransition = error "TODO find out when the transition can happen"

-- transition when the major part of the version number is incremented
-- hard code that value
--
-- proposed, voted, endorsed
--
-- happens on epoch boundary: current epoch + 1
-- needs to be stable
--
-- https://github.com/input-output-hk/ouroboros-network/blob/b7e863041ec1db12ee4726fc7252f1db3721aedd/ouroboros-consensus/docs/HardFork.md

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Byron"
    }

{-------------------------------------------------------------------------------
  SingleEraBlock Shelley
-------------------------------------------------------------------------------}

instance SingleEraBlock (ShelleyBlock TPraosStandardCrypto) where
  singleEraParams _ = shelleyLedgerEraParams

  -- No transition from Shelley to Goguen yet
  singleEraTransition _cfg _st = Nothing

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Shelley"
    }

{-------------------------------------------------------------------------------
  Translation from Byron to Shelley
-------------------------------------------------------------------------------}

translateHeaderHashFromByronToShelley
  :: HeaderHash ByronBlock
  -> HeaderHash (ShelleyBlock TPraosStandardCrypto)
translateHeaderHashFromByronToShelley =
      ShelleyHash
    . SL.HashHeader
      -- We use the same hashing algorithm, so this is safe
    . Hash.UnsafeHash
    . Hashing.hashToBytes
    . unByronHash

translatePointFromByronToShelley
  :: Point ByronBlock
  -> Point (ShelleyBlock TPraosStandardCrypto)
translatePointFromByronToShelley = \case
    GenesisPoint   -> GenesisPoint
    BlockPoint s h -> BlockPoint s (translateHeaderHashFromByronToShelley h)

translateUTxOFromByronToShelley
  :: CC.UTxO
  -> SL.UTxO TPraosStandardCrypto
translateUTxOFromByronToShelley (CC.UTxO byronUTxO) =
    SL.UTxO $ Map.fromList
      [ (shelleyTxIn, shelleyTxOut)
      | (byronTxIn, byronTxOut) <- Map.toList byronUTxO
      , let shelleyTxIn  = translateTxIn  $ CC.fromCompactTxIn  byronTxIn
            shelleyTxOut = translateTxOut $ CC.fromCompactTxOut byronTxOut
      ]
  where
    translateTxIn :: CC.TxIn -> SL.TxIn TPraosStandardCrypto
    translateTxIn (CC.TxInUtxo txId idx) =
      SL.TxIn (translateTxId txId) (fromIntegral idx)

    translateTxOut :: CC.TxOut -> SL.TxOut TPraosStandardCrypto
    translateTxOut (CC.TxOut addr amount) =
      SL.TxOut (translateAddr addr) (translateAmount amount)

    -- | We use the same hasing algorithm so we can unwrap and rewrap the
    -- bytes. We don't care about the type that is hashed, which will differ
    -- going from Byron to Shelley, we just use the hashes as IDs.
    translateTxId :: CC.TxId -> SL.TxId TPraosStandardCrypto
    translateTxId = SL.TxId . Hash.UnsafeHash . Hashing.hashToBytes

    translateAmount :: CC.Lovelace -> SL.Coin
    translateAmount = SL.Coin . CC.lovelaceToInteger

    translateAddr :: CC.Address -> SL.Addr TPraosStandardCrypto
    translateAddr = undefined

translateLedgerStateFromByronToShelley
  :: LedgerConfig ByronBlock
  -> LedgerConfig (ShelleyBlock TPraosStandardCrypto)
  -> EpochNo
  -> LedgerState  ByronBlock
  -> LedgerState  (ShelleyBlock TPraosStandardCrypto)
translateLedgerStateFromByronToShelley _ shelleyCfg epochNo byronLedger =
    ShelleyLedgerState {
        ledgerTip = shelleyLedgerTip
      , history   = History.empty
      , shelleyState
      }
  where
    ShelleyLedgerConfig { shelleyLedgerGenesis } = shelleyCfg

    shelleyState :: SL.ShelleyState TPraosStandardCrypto
    shelleyState = SL.NewEpochState {
        nesEL     = epochNo
      , nesBprev  = SL.BlocksMade Map.empty
      , nesBcur   = SL.BlocksMade Map.empty
      , nesEs     = epochState
      , nesRu     = SL.SNothing
      , nesPd     = SL.PoolDistr Map.empty
      , nesOsched = overlaySchedule
      }

    pparams :: SL.PParams
    pparams = sgProtocolParams shelleyLedgerGenesis

    -- | NOTE: we ignore the Byron delegation map because the genesis and
    -- delegation verification keys are hashed using a different hashing
    -- scheme, Blake2b_224, whereas Shelley uses Blake2b_256. This means we
    -- can't simply convert them, as Byron nowhere stores the original
    -- verification keys.
    --
    -- Fortunately, no Byron genesis delegations have happened yet, and if
    -- they did, we would be aware of them before the hard fork, as we
    -- instigate the hard fork. We just have to make sure that the hard-coded
    -- Shelley genensis contains the same genesis and delegation verification
    -- keys, but hashed with the right algorithm.
    genDelegs :: SL.GenDelegs TPraosStandardCrypto
    genDelegs = SL.GenDelegs $ sgGenDelegs shelleyLedgerGenesis

    reserves :: SL.Coin
    reserves = undefined

    epochState :: SL.EpochState TPraosStandardCrypto
    epochState = SL.EpochState {
        esAccountState = SL.AccountState (SL.Coin 0) reserves
      , esSnapshots    = SL.emptySnapShots
      , esLState       = ledgerState
      , esPrevPp       = pparams
      , esPp           = pparams
      , esNonMyopic    = SL.emptyNonMyopic
      }

    byronUtxO :: CC.UTxO
    byronUtxO = CC.cvsUtxo $ byronLedgerState byronLedger

    shelleyUtxo :: SL.UTxO TPraosStandardCrypto
    shelleyUtxo = translateUTxOFromByronToShelley byronUtxO

    ledgerState :: SL.LedgerState TPraosStandardCrypto
    ledgerState = SL.LedgerState {
        _utxoState = SL.UTxOState {
            _utxo      = shelleyUtxo
          , _deposited = SL.Coin 0
          , _fees      = SL.Coin 0
          , _ppups     = SL.ProposedPPUpdates Map.empty
          }
      , _delegationState = SL.DPState {
          _dstate = SL.emptyDState { SL._genDelegs = genDelegs }
        , _pstate = SL.emptyPState
        }
      }

    shelleyLedgerTip :: Point (ShelleyBlock TPraosStandardCrypto)
    shelleyLedgerTip =
      -- TODO take the hash of the Shelley genesis config into account: hash
      -- the concatenation of the last Byron hash with the Shelley genesis
      -- config hash
      translatePointFromByronToShelley $
      ledgerTipPoint' (Proxy @ByronBlock) byronLedger

    overlaySchedule :: Map SlotNo (SL.OBftSlot TPraosStandardCrypto)
    overlaySchedule =
      flip runReader (shelleyLedgerGlobals shelleyCfg) $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs shelleyLedgerGenesis))
          (sgProtocolParams shelleyLedgerGenesis)

translateConsensusStateFromByronToShelley
  :: ConsensusConfig (BlockProtocol ByronBlock)
  -> ConsensusConfig (BlockProtocol (ShelleyBlock TPraosStandardCrypto))
  -> ByteString  -- ^ Entropy
  -> PBftState   PBftByronCrypto
  -> TPraosState TPraosStandardCrypto
translateConsensusStateFromByronToShelley _ _ entropy pbftState =
    TPraosState.empty (PBftState.tipSlot pbftState) $
      SL.PrtclState
        Map.empty
        -- TODO correct nonces?
        nonce
        nonce
        nonce
        nonce
        nonce
  where
    nonce = SL.Nonce (coerce entropy)

-- | We ignore 'PBftLedgerView' entirely and construct a 'SL.LedgerView' using
-- the Shelley genesis config in the same way as
-- 'translateLedgerStateFromByronToShelley'.
translateLedgerViewFromByronToShelley
  :: LedgerConfig ByronBlock
  -> LedgerConfig (ShelleyBlock TPraosStandardCrypto)
  -> EpochNo
  -> PBftLedgerView PBftByronCrypto
  -> SL.LedgerView TPraosStandardCrypto
translateLedgerViewFromByronToShelley _ shelleyCfg _pbftLedgerView =
    tpraosLedgerView
  where
    ShelleyLedgerConfig { shelleyLedgerGenesis, shelleyLedgerGlobals } = shelleyCfg

    -- Which epoch are we in?
    epochNo = undefined

    tpraosLedgerView = SL.LedgerView {
        lvProtParams   = sgProtocolParams shelleyLedgerGenesis
      , lvOverlaySched = overlaySchedule
      , lvPoolDistr    = SL.PoolDistr Map.empty
      , lvGenDelegs    = SL.GenDelegs $ sgGenDelegs shelleyLedgerGenesis
      }

    overlaySchedule :: Map SlotNo (SL.OBftSlot TPraosStandardCrypto)
    overlaySchedule =
      flip runReader shelleyLedgerGlobals $
        SL.overlaySchedule
          epochNo
          (Map.keysSet (sgGenDelegs shelleyLedgerGenesis))
          (sgProtocolParams shelleyLedgerGenesis)


{-------------------------------------------------------------------------------
  CanHardFork instance
-------------------------------------------------------------------------------}

checkTransitionFromByronToShelley
  :: HeaderHash ByronBlock
  -> ChainHash (ShelleyBlock TPraosStandardCrypto)
  -> Bool
checkTransitionFromByronToShelley (ByronHash lastByronHash) = \case
    GenesisHash ->
      -- The previous block of the first Shelley block is genesis. We have the
      -- hash of the real last Byron block, so clearly they don't match.
      False
    BlockHash (ShelleyHash (SL.HashHeader firstShelleyPrevHash)) ->
      -- While Byron and Shelley use the same hashing algorithm, they are
      -- different types, so extract the raw bytes and compare them.
      Hashing.hashToBytes lastByronHash == Hash.getHash firstShelleyPrevHash

instance CanHardFork CardanoEras where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState =
        InPairs.PCons
          (TranslateEraLedgerState translateLedgerStateFromByronToShelley)
          InPairs.PNil
    , translateConsensusState =
        InPairs.PCons
          (TranslateEraConsensusState translateConsensusStateFromByronToShelley)
          InPairs.PNil
    , translateLedgerView =
        InPairs.PCons
          (TranslateEraLedgerView translateLedgerViewFromByronToShelley)
          InPairs.PNil
    }

  hardForkEraTransitionCheck = EraTransitionCheck {
      getCheckEraTransition =
        InPairs.PCons
          (CheckTransition checkTransitionFromByronToShelley)
          InPairs.PNil
    }

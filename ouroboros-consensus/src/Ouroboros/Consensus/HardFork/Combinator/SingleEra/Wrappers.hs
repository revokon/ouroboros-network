{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Newtype wrappers around type families
module Ouroboros.Consensus.HardFork.Combinator.SingleEra.Wrappers (
    -- * Block based
    SingleEraHash(..)
  , SingleEraTipInfo(..)
  , SingleEraEnvelopeErr(..)
  , SingleEraLedgerConfig(..)
  , SingleEraLedgerError(..)
  , SingleEraGenTxId(..)
  , SingleEraApplyTxErr(..)
    -- * Protocol based
  , SingleEraConsensusConfig(..)
  , SingleEraIsLeader(..)
  , SingleEraLedgerView(..)
  , SingleEraValidationErr(..)
  , SingleEraValidateView(..)
  , SingleEraSelectView(..)
    -- * EraInfo
  , LedgerEraInfo(..)
  ) where

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra.Info

{-------------------------------------------------------------------------------
  Newtype wrappers around type families

  We need these because type families cannot be partially applied.
-------------------------------------------------------------------------------}

newtype SingleEraHash         blk = SingleEraHash         { getSingleEraHash         :: HeaderHash               blk }
newtype SingleEraTipInfo      blk = SingleEraTipInfo      { getSingleEraTipInfo      :: TipInfo                  blk }
newtype SingleEraEnvelopeErr  blk = SingleEraEnvelopeErr  { getSingleEraEnvelopeErr  :: OtherHeaderEnvelopeError blk }
newtype SingleEraLedgerConfig blk = SingleEraLedgerConfig { getSingleEraLedgerConfig :: LedgerConfig             blk }
newtype SingleEraLedgerError  blk = SingleEraLedgerError  { getSingleEraLedgerError  :: LedgerError              blk }
newtype SingleEraGenTxId      blk = SingleEraGenTxId      { getSingleEraGenTxId      :: GenTxId                  blk }
newtype SingleEraApplyTxErr   blk = SingleEraApplyTxErr   { getSingleEraApplyTxErr   :: ApplyTxErr               blk }

{-------------------------------------------------------------------------------
  Consensus based
-------------------------------------------------------------------------------}

newtype SingleEraConsensusConfig blk = SingleEraConsensusConfig { getSingleEraConsensusConfig :: ConsensusConfig (BlockProtocol blk) }
newtype SingleEraIsLeader        blk = SingleEraIsLeader        { getSingleEraIsLeader        :: IsLeader        (BlockProtocol blk) }
newtype SingleEraLedgerView      blk = SingleEraLedgerView      { getSingleEraLedgerView      :: LedgerView      (BlockProtocol blk) }
newtype SingleEraValidationErr   blk = SingleEraValidationErr   { getSingleEraValidationErr   :: ValidationErr   (BlockProtocol blk) }
newtype SingleEraValidateView    blk = SingleEraValidateView    { getSingleEraValidateView    :: ValidateView    (BlockProtocol blk) }
newtype SingleEraSelectView      blk = SingleEraSelectView      { getSingleEraSelectView      :: SelectView      (BlockProtocol blk) }

{-------------------------------------------------------------------------------
  Additional newtype wrappers around 'EraInfo'

  These are useful primarily for use in error messages.

  The construction functions ('singleEraInfoLedger' and co) don't take a
  'Proxy' but rather a more precise type primarily to avoid type errors (which
  are easy to make because 'LedgerEraInfo's type parameter is phantom).
-------------------------------------------------------------------------------}

newtype LedgerEraInfo blk = LedgerEraInfo {
      getLedgerEraInfo :: SingleEraInfo blk
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraHash blk)
deriving newtype instance SingleEraBlock blk => Ord                (SingleEraHash blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraHash blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraHash blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraTipInfo blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraTipInfo blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraTipInfo blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraEnvelopeErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraEnvelopeErr blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraGenTxId blk)
deriving newtype instance SingleEraBlock blk => Ord                (SingleEraGenTxId blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraGenTxId blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraLedgerError blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraLedgerError blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraLedgerError blk)

deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraConsensusConfig blk)

deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraLedgerConfig blk)

deriving newtype instance SingleEraBlock blk => Show               (SingleEraLedgerView blk)

deriving newtype instance SingleEraBlock blk => Eq                 (SingleEraValidationErr blk)
deriving newtype instance SingleEraBlock blk => Show               (SingleEraValidationErr blk)
deriving newtype instance SingleEraBlock blk => NoUnexpectedThunks (SingleEraValidationErr blk)

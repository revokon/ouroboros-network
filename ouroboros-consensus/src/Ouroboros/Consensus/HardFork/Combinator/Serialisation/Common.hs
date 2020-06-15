{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common (
    -- * Conditions required by the HFC to support serialisation
    SerialiseHFC(..)
  , SerialiseConstraintsHFC
  , pSHFC
  , FutureEraException(..)
  , futureEraException
    -- * Distinguish first era from the rest
  , FirstEra
  , LaterEra
  , isFirstEra
  , notFirstEra
    -- * Versioning
  , HardForkNodeToNodeVersion(..)
  , HardForkNodeToClientVersion(..)
    -- * Dealing with annotations
  , AnnDecoder(..)
    -- * Serialisation of telescopes
  , encodeTelescope
  , decodeTelescope
    -- * Serialisation of sums
  , encodeNS
  , decodeNS
  , decodeAnnNS
    -- * Dependent serialisation
  , encodeNestedCtxt
  , decodeNestedCtxt
  , encodeNested
  , decodeNested
    -- * MismatchEraInfo
  , encodeEitherMismatch
  , decodeEitherMismatch
    -- * Distributive properties
  , distribAnnTip
  , undistribAnnTip
  , distribSerialisedHeader
  , undistribSerialisedHeader
  , distribSomeQuery
  , undistribSomeQuery
    -- * Deriving-via support for tests
  , SerialiseNS(..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Control.Exception (Exception)
import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as Lazy
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.SOP.Strict
import           Data.Word

import           Cardano.Binary (enforceSize)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.State
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (SimpleTelescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ChainDB.Serialisation
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

{-------------------------------------------------------------------------------
  Distinguish between the first era and all others
-------------------------------------------------------------------------------}

type family FirstEra (xs :: [*]) where
  FirstEra (x ': xs) = x

type family LaterEra (xs :: [*]) where
  LaterEra (x ': xs) = xs

isFirstEra :: forall f xs. All SingleEraBlock xs
           => NS f xs
           -> Either (NS SingleEraInfo (LaterEra xs)) (f (FirstEra xs))
isFirstEra (Z x) = Right x
isFirstEra (S x) = Left (hcmap proxySingle aux x)
  where
    aux :: forall blk. SingleEraBlock blk => f blk -> SingleEraInfo blk
    aux _ = singleEraInfo (Proxy @blk)

-- | Used to construct 'FutureEraException'
notFirstEra :: All SingleEraBlock xs
            => NS f xs -- ^ 'NS' intended to be from a future era
            -> NS SingleEraInfo xs
notFirstEra = hcmap proxySingle aux
  where
    aux :: forall f blk. SingleEraBlock blk => f blk -> SingleEraInfo blk
    aux _ = singleEraInfo (Proxy @blk)

{-------------------------------------------------------------------------------
  Versioning
-------------------------------------------------------------------------------}

data HardForkNodeToNodeVersion xs where
  HardForkNodeToNodeDisabled :: WrapNodeToNodeVersion x -> HardForkNodeToNodeVersion (x ': xs)
  HardForkNodeToNodeEnabled  :: NP WrapNodeToNodeVersion xs -> HardForkNodeToNodeVersion xs

data HardForkNodeToClientVersion xs where
  HardForkNodeToClientDisabled :: WrapNodeToClientVersion x -> HardForkNodeToClientVersion (x ': xs)
  HardForkNodeToClientEnabled  :: NP WrapNodeToClientVersion xs -> HardForkNodeToClientVersion xs

deriving instance All (Compose Show WrapNodeToNodeVersion)   xs => Show (HardForkNodeToNodeVersion xs)
deriving instance All (Compose Show WrapNodeToClientVersion) xs => Show (HardForkNodeToClientVersion xs)

deriving instance All (Compose Eq WrapNodeToNodeVersion)   xs => Eq (HardForkNodeToNodeVersion xs)
deriving instance All (Compose Eq WrapNodeToClientVersion) xs => Eq (HardForkNodeToClientVersion xs)

instance SerialiseHFC xs => HasNetworkProtocolVersion (HardForkBlock xs) where
  type BlockNodeToNodeVersion   (HardForkBlock xs) = HardForkNodeToNodeVersion   xs
  type BlockNodeToClientVersion (HardForkBlock xs) = HardForkNodeToClientVersion xs

{-------------------------------------------------------------------------------
  Conditions required by the HFC to support serialisation
-------------------------------------------------------------------------------}

class ( SingleEraBlock                   blk
      , SerialiseDiskConstraints         blk
      , SerialiseNodeToNodeConstraints   blk
      , SerialiseNodeToClientConstraints blk
      , HasNetworkProtocolVersion        blk
      ) => SerialiseConstraintsHFC       blk

pSHFC :: Proxy SerialiseConstraintsHFC
pSHFC = Proxy

-- | Conditions required by the HFC to provide serialisation
--
-- NOTE: Compatibility between HFC enabled and disabled:
--
-- 1. Node-to-node and node-to-client communication is versioned. When the HFC
--    is disabled, we default to the instances for the first era, and so
--    compatibility is preserved by construction.
--
-- 2. On-disk storage is /not/ versioned, and here we make no attempt to be
--    compatible between non-HFC and HFC deployments, /except/ for blocks: we
--    define two methods 'encodeDiskHfcBlock' and 'decodeDiskHfcBlock' which
--    are used for on-disk serialisation of blocks. These methods have
--    defaults which can and probably should be used for deployments that use
--    the HFC from the get-go, but for deployments that only later change to use
--    the HFC these functions can be overriden to provide an on-disk storage
--    format for HFC blocks that is compatible with the on-disk storage of
--    blocks from the first era.
--
-- 3. The converse is NOT supported. Deployments that use the HFC from the start
--    should not use 'HardForkNodeToNodeDisabled' and/or
--    'HardForkNodeToClientDisabled'. Doing so would result in opposite
--    compatibility problems: the on-disk block would include the HFC tag, but
--    sending blocks with the HFC disabled suggests that that tag is unexpected.
--    This would then lead to problems with binary streaming, and we do not
--    currently provide any provisions to resolve these.
class ( CanHardFork xs
      , All SerialiseConstraintsHFC xs
        -- Required for HasNetworkProtocolVersion
      , All (Compose Show WrapNodeToNodeVersion)   xs
      , All (Compose Eq   WrapNodeToNodeVersion)   xs
      , All (Compose Show WrapNodeToClientVersion) xs
      , All (Compose Eq   WrapNodeToClientVersion) xs
        -- Required for 'encodeNestedCtxt'/'decodeNestedCtxt'
      , All (EncodeDiskDepIx (NestedCtxt Header)) xs
      , All (DecodeDiskDepIx (NestedCtxt Header)) xs
      ) => SerialiseHFC xs where

  encodeDiskHfcBlock :: CodecConfig (HardForkBlock xs)
                     -> HardForkBlock xs -> Encoding
  encodeDiskHfcBlock cfg =
        encodeNS (hcmap pSHFC (fn . mapIK . encodeDisk) cfgs)
      . (getOneEraBlock . getHardForkBlock)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

  decodeDiskHfcBlock :: CodecConfig (HardForkBlock xs)
                     -> forall s. Decoder s (Lazy.ByteString -> HardForkBlock xs)
  decodeDiskHfcBlock cfg =
        fmap (\f -> HardForkBlock . OneEraBlock . f)
      $ decodeAnnNS (hcmap pSHFC aux cfgs)
    where
      cfgs = getPerEraCodecConfig (hardForkCodecConfigPerEra cfg)

      aux :: SerialiseDiskConstraints blk
          => CodecConfig blk -> AnnDecoder I blk
      aux cfg' = AnnDecoder $ (\f -> I . f) <$> decodeDisk cfg'

  -- | Used as the implementation of 'reconstructPrefixLen' for
  -- 'HardForkBlock'.
  reconstructHfcPrefixLen :: proxy (Header (HardForkBlock xs)) -> Word8
  reconstructHfcPrefixLen _ =
      -- We insert two bytes at the front
      2 + maximum (hcollapse perEra)
    where
      perEra :: NP (K Word8) xs
      perEra = hcpure proxySingle reconstructOne

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K Word8 blk
      reconstructOne = K $ reconstructPrefixLen (Proxy @(Header blk))

  -- | Used as the implementation of 'reconstructNestedCtxt' for
  -- 'HardForkBlock'.
  reconstructHfcNestedCtxt ::
       proxy (Header (HardForkBlock xs))
    -> ShortByteString  -- ^ First bytes ('reconstructPrefixLen') of the block
    -> SizeInBytes      -- ^ Block size
    -> SomeBlock (NestedCtxt Header) (HardForkBlock xs)
  reconstructHfcNestedCtxt _ prefix blockSize =
     case nsFromIndex tag of
       Nothing -> error $ "invalid HardForkBlock with tag: " <> show tag
       Just ns -> injSomeBlock $ hcmap proxySingle reconstructOne ns
    where
      tag :: Word8
      tag = Short.index prefix 1

      prefixOne :: ShortByteString
      prefixOne = Short.pack . drop 2 . Short.unpack $ prefix

      reconstructOne :: forall blk. SingleEraBlock blk
                     => K () blk -> SomeBlock (NestedCtxt Header) blk
      reconstructOne _ =
          reconstructNestedCtxt (Proxy @(Header blk)) prefixOne blockSize

      injSomeBlock :: NS (SomeBlock (NestedCtxt Header)) xs'
                   -> SomeBlock (NestedCtxt Header) (HardForkBlock xs')
      injSomeBlock (Z x) = case x of
          SomeBlock (NestedCtxt y) -> SomeBlock (NestedCtxt (NCZ y))
      injSomeBlock (S x) = case injSomeBlock x of
          SomeBlock (NestedCtxt y) -> SomeBlock (NestedCtxt (NCS y))

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Thrown in the node-to-node and node-to-client encoders when the HFC
-- is disabled but we see something from an era that is not the first
data FutureEraException where
  -- | We record from which era we saw something
  FutureEraException :: SingleEraInfo blk -> FutureEraException

deriving instance Show FutureEraException
instance Exception FutureEraException

futureEraException :: SListI xs => NS SingleEraInfo xs -> FutureEraException
futureEraException = hcollapse . hmap (K . FutureEraException)

{-------------------------------------------------------------------------------
  Dealing with annotations
-------------------------------------------------------------------------------}

data AnnDecoder f blk = AnnDecoder {
      annDecoder :: forall s. Decoder s (Lazy.ByteString -> f blk)
    }

{-------------------------------------------------------------------------------
  Serialisation of telescopes
-------------------------------------------------------------------------------}

encodeTelescope :: SListI xs
                => NP (f -.-> K Encoding) xs -> HardForkState f xs -> Encoding
encodeTelescope es (HardForkState st) = mconcat [
      Enc.encodeListLen (1 + fromIntegral ix)
    , mconcat $ hcollapse $ SimpleTelescope $
        (Telescope.bihzipWith encPast encCurrent es st)
    ]
  where
    -- The tip of the telescope also tells us the length
    ix :: Word8
    ix = hcollapse $ hzipWith const npWithIndices (Telescope.tip st)

    encPast :: (f -.-> K Encoding) blk -> Past f blk -> K Encoding blk
    encPast enc = K . encodePast (unK . apFn enc)

    encCurrent :: (f -.-> K Encoding) blk -> Current f blk  -> K Encoding blk
    encCurrent enc = K . encodeCurrent (unK . apFn enc)

decodeTelescope :: NP (Decoder s :.: f) xs -> Decoder s (HardForkState f xs)
decodeTelescope = \ds -> do
    ix <- Dec.decodeListLen
    if ix < 1
      then fail $ "decodeTelescope: invalid telescope length " ++ show ix
      else HardForkState <$> go (ix - 1) ds
  where
    go :: Int
       -> NP (Decoder s :.: f) xs
       -> Decoder s (Telescope (Past f) (Current f) xs)
    go 0 (Comp d :* _)  = TZ <$> decodeCurrent d
    go i (Comp d :* ds) = TS <$> decodePast d <*> go (i - 1) ds
    go _ Nil            = error "decodeTelescope: invalid telescope length"

{-------------------------------------------------------------------------------
  Serialisation of sums
-------------------------------------------------------------------------------}

encodeNS :: SListI xs => NP (f -.-> K Encoding) xs -> NS f xs -> Encoding
encodeNS es ns = mconcat [
      Enc.encodeListLen 2
    , Enc.encodeWord8 $ hcollapse $ hzipWith const npWithIndices ns
    , hcollapse $ hzipWith apFn es ns
    ]

decodeNS :: SListI xs => NP (Decoder s :.: f) xs -> Decoder s (NS f xs)
decodeNS ds = do
    enforceSize "decodeNS" 2
    i <- Dec.decodeWord8
    case nsFromIndex i of
      Nothing -> fail $ "decodeNS: invalid index " ++ show i
      Just ns -> hcollapse $ hzipWith3 aux injections ds ns
  where
    aux :: (f -.-> K (NS f xs)) blk
        -> (Decoder s :.: f) blk
        -> K () blk
        -> K (Decoder s (NS f xs)) blk
    aux inj (Comp dec) (K ()) = K $ (unK . apFn inj) <$> dec

decodeAnnNS :: SListI xs
            => NP (AnnDecoder f) xs
            -> forall s. Decoder s (Lazy.ByteString -> NS f xs)
decodeAnnNS ds = do
    enforceSize "decodeDiskAnnNS" 2
    i <- Dec.decodeWord8
    case nsFromIndex i of
      Nothing -> fail $ "decodeAnnNS: invalid index " ++ show i
      Just ns -> hcollapse $ hzipWith3 aux injections ds ns
  where
    aux :: (f -.-> K (NS f xs)) blk
        -> AnnDecoder f blk
        -> K () blk
        -> K (Decoder s (Lazy.ByteString -> NS f xs)) blk
    aux inj (AnnDecoder dec) (K ()) = K $
       (\f -> unK . apFn inj . f ) <$> dec

{-------------------------------------------------------------------------------
  Dependent serialisation
-------------------------------------------------------------------------------}

encodeNested :: All (EncodeDiskDep (NestedCtxt f)) xs
             => CodecConfig (HardForkBlock xs)
             -> NestedCtxt f (HardForkBlock xs) a
             -> a
             -> Encoding
encodeNested = \ccfg (NestedCtxt ctxt) a ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ctxt a
  where
    go :: All (EncodeDiskDep (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> a -> Encoding
    go Nil       ctxt       = case ctxt of {}
    go (c :* _)  (NCZ ctxt) = encodeDiskDep c (NestedCtxt ctxt)
    go (_ :* cs) (NCS ctxt) = go cs ctxt

decodeNested :: All (DecodeDiskDep (NestedCtxt f)) xs
             => CodecConfig (HardForkBlock xs)
             -> NestedCtxt f (HardForkBlock xs) a
             -> forall s. Decoder s (Lazy.ByteString -> a)
decodeNested = \ccfg (NestedCtxt ctxt) ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ctxt
  where
    go :: All (DecodeDiskDep (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> Decoder s (Lazy.ByteString -> a)
    go Nil       ctxt       = case ctxt of {}
    go (c :* _)  (NCZ ctxt) = decodeDiskDep c (NestedCtxt ctxt)
    go (_ :* cs) (NCS ctxt) = go cs ctxt

encodeNestedCtxt :: All (EncodeDiskDepIx (NestedCtxt f)) xs
                 => CodecConfig (HardForkBlock xs)
                 -> SomeBlock (NestedCtxt f) (HardForkBlock xs)
                 -> Encoding
encodeNestedCtxt = \ccfg (SomeBlock ctxt) ->
    go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg))
       npWithIndices
       (flipNestedCtxt ctxt)
  where
    go :: All (EncodeDiskDepIx (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NP (K Word8) xs'
       -> NestedCtxt_ (HardForkBlock xs') f a
       -> Encoding
    go Nil       _           ctxt       = case ctxt of {}
    go (_ :* cs) (_   :* is) (NCS ctxt) = go cs is ctxt
    go (c :* _)  (K i :* _)  (NCZ ctxt) = mconcat [
          Enc.encodeListLen 2
        , Serialise.encode i
        , encodeDiskDepIx c (SomeBlock (NestedCtxt ctxt))
        ]

decodeNestedCtxt :: All (DecodeDiskDepIx (NestedCtxt f)) xs
                 => CodecConfig (HardForkBlock xs)
                 -> forall s. Decoder s (SomeBlock (NestedCtxt f) (HardForkBlock xs))
decodeNestedCtxt = \ccfg -> do
    enforceSize "decodeNestedCtxt" 2
    tag <- Serialise.decode
    case nsFromIndex tag of
      Nothing -> fail $ "decodeNestedCtxt: invalid tag " ++ show tag
      Just ns ->
        go (getPerEraCodecConfig (hardForkCodecConfigPerEra ccfg)) ns
  where
    go :: All (DecodeDiskDepIx (NestedCtxt f)) xs'
       => NP CodecConfig xs'
       -> NS (K ()) xs'
       -> forall s. Decoder s (SomeBlock (NestedCtxt f) (HardForkBlock xs'))
    go Nil       i     = case i of {}
    go (c :* _)  (Z _) = mapSomeNestedCtxt NCZ <$> decodeDiskDepIx c
    go (_ :* cs) (S i) = mapSomeNestedCtxt NCS <$> go cs i

{-------------------------------------------------------------------------------
  Serialisation of 'MismatchEraInfo'

  We have to be careful here not to introduce any additional wrapping when
  using 'HardForkNodeToClientDisabled'.
-------------------------------------------------------------------------------}

encodeEitherMismatch :: forall xs a. SListI xs
                     => BlockNodeToClientVersion (HardForkBlock xs)
                     -> (a -> Encoding)
                     -> (Either (MismatchEraInfo xs) a -> Encoding)
encodeEitherMismatch version enc ma =
    case (version, ma) of
      (HardForkNodeToClientDisabled _, Right a) ->
          enc a
      (HardForkNodeToClientDisabled _, Left err) ->
          throw $ futureEraException (mismatchFutureEra err)
      (HardForkNodeToClientEnabled _, Right a) -> mconcat [
            Enc.encodeListLen 1
          , enc a
          ]
      (HardForkNodeToClientEnabled _, Left (MismatchEraInfo err)) -> mconcat [
            Enc.encodeListLen 2
          , encodeNS (hpure (fn encodeName)) era1
          , encodeNS (hpure (fn (encodeName . getLedgerEraInfo))) era2
          ]
        where
          era1 :: NS SingleEraInfo xs
          era2 :: NS LedgerEraInfo xs
          (era1, era2) = Match.mismatchToNS err
  where
    encodeName :: SingleEraInfo blk -> K Encoding blk
    encodeName = K . Serialise.encode . singleEraName

decodeEitherMismatch :: SListI xs
                     => BlockNodeToClientVersion (HardForkBlock xs)
                     -> Decoder s a
                     -> Decoder s (Either (MismatchEraInfo xs) a)
decodeEitherMismatch version dec =
    case version of
      HardForkNodeToClientDisabled _ ->
        Right <$> dec
      HardForkNodeToClientEnabled _ -> do
        tag <- Dec.decodeListLen
        case tag of
          1 -> Right <$> dec
          2 -> do era1 <- decodeNS (hpure (Comp decodeName))
                  era2 <- decodeNS (hpure (Comp (LedgerEraInfo <$> decodeName)))
                  case Match.matchNS era1 era2 of
                    Left err -> return $ Left (MismatchEraInfo err)
                    Right _  -> fail "dispatchDecoderErr: unexpected match"
          _ -> fail $ "dispatchDecoderErr: invalid tag " ++ show tag
  where
    decodeName :: forall blk s. Decoder s (SingleEraInfo blk)
    decodeName = SingleEraInfo <$> Serialise.decode

{-------------------------------------------------------------------------------
  Distributive properties
-------------------------------------------------------------------------------}

distribAnnTip :: SListI xs => AnnTip (HardForkBlock xs) -> NS AnnTip xs
distribAnnTip AnnTip{..} =
    hmap distrib (getOneEraTipInfo annTipInfo)
  where
    distrib :: WrapTipInfo blk -> AnnTip blk
    distrib (WrapTipInfo info) =
        AnnTip annTipSlotNo annTipBlockNo info

undistribAnnTip :: SListI xs => NS AnnTip xs -> AnnTip (HardForkBlock xs)
undistribAnnTip = hcollapse . hzipWith undistrib injections
  where
    undistrib :: (WrapTipInfo -.-> K (NS WrapTipInfo xs)) blk
              -> AnnTip blk
              -> K (AnnTip (HardForkBlock xs)) blk
    undistrib inj AnnTip{..} = K $
        AnnTip annTipSlotNo
               annTipBlockNo
               (OneEraTipInfo $ unK . apFn inj . WrapTipInfo $ annTipInfo)

distribSerialisedHeader :: SerialisedHeader (HardForkBlock xs)
                        -> NS SerialisedHeader xs
distribSerialisedHeader = \hdr ->
    case serialisedHeaderToDepPair hdr of
      GenDepPair (NestedCtxt ctxt) bs ->
        go ctxt bs
  where
    go :: NestedCtxt_ (HardForkBlock xs) Header a
       -> Serialised a
       -> NS SerialisedHeader xs
    go (NCZ c) = Z . SerialisedHeaderFromDepPair . GenDepPair (NestedCtxt c)
    go (NCS c) = S . go c

undistribSerialisedHeader :: NS SerialisedHeader xs
                          -> SerialisedHeader (HardForkBlock xs)
undistribSerialisedHeader =
    SerialisedHeaderFromDepPair . go
  where
    go :: NS SerialisedHeader xs
       -> GenDepPair Serialised (NestedCtxt Header (HardForkBlock xs))
    go (Z (SerialisedHeaderFromDepPair (GenDepPair (NestedCtxt c) bs))) =
        GenDepPair (NestedCtxt (NCZ c)) bs
    go (S bs) =
        depPairFirst (mapNestedCtxt NCS) $ go bs

distribSomeQuery :: SomeBlock Query (HardForkBlock xs)
                 -> NS (SomeBlock Query) xs
distribSomeQuery = \(SomeBlock (HardForkQuery qry)) ->
    go qry
  where
    go :: HardForkQuery xs result -> NS (SomeBlock Query) xs
    go (QZ qry) = Z (SomeBlock qry)
    go (QS qry) = S (go qry)

undistribSomeQuery :: NS (SomeBlock Query) xs
                   -> SomeBlock Query (HardForkBlock xs)
undistribSomeQuery = go
  where
    go :: NS (SomeBlock Query) xs
       -> SomeBlock Query (HardForkBlock xs)
    go (Z qry) = case qry of
                   SomeBlock qry' ->
                     SomeBlock $ HardForkQuery (QZ qry')
    go (S qry) = case go qry of
                   SomeBlock (HardForkQuery qry') ->
                     SomeBlock $ HardForkQuery (QS qry')

{-------------------------------------------------------------------------------
  Deriving-via support

  This is primarily for the benefit of tests, and depends only on 'Serialise'
  (rather than 'SerialiseDisk'/'SerialiseNodeToNode'/'SerialiseNodeToClient').
-------------------------------------------------------------------------------}

-- | Used for deriving via
--
-- Example
--
-- > deriving via SerialiseNS Header SomeEras
-- >          instance Serialise (Header SomeBlock)
newtype SerialiseNS f xs = SerialiseNS {
      getSerialiseNS :: NS f xs
    }

instance All (Compose Serialise f) xs => Serialise (SerialiseNS f xs) where
  encode = encodeNS (hcpure (Proxy @(Compose Serialise f))
                            (fn (K . Serialise.encode)))
         . getSerialiseNS

  decode = fmap SerialiseNS
         $ decodeNS (hcpure (Proxy @(Compose Serialise f))
                            (Comp Serialise.decode))
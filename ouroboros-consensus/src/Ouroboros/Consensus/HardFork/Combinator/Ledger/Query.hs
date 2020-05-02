{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
    Query(..)
  , HardForkQuery(..)
  , HardForkQueryResult
  , getHardForkQuery
  ) where

import           Data.Bifunctor
import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP
import           Data.Type.Equality

import           Ouroboros.Consensus.Ledger.Abstract

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Config
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Current
                     (CurrentLedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.State
                     (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.SingleEra
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match
                     (Mismatch (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

instance CanHardFork xs => ShowQuery (Query (HardForkBlock xs)) where
  showResult = \(HardForkQuery qry) mResult ->
      case mResult of
        Left  err    -> show err
        Right result -> go qry result
    where
      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result -> result -> String
      go (QZ qry) = showResult qry
      go (QS qry) = go qry

type HardForkQueryResult xs = Either (MismatchEraInfo xs)

instance CanHardFork xs => QueryLedger (HardForkBlock xs) where
  data Query (HardForkBlock xs) :: * -> * where
    HardForkQuery :: HardForkQuery xs result
                  -> Query (HardForkBlock xs) (HardForkQueryResult xs result)

  answerQuery HardForkLedgerConfig{..} = \(HardForkQuery qry)
                                          (HardForkLedgerState st) ->
      go qry (hzipWith Pair cfgs (Telescope.tip st))
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result
         -> NS (Product SingleEraLedgerConfig CurrentLedgerState) xs'
         -> HardForkQueryResult xs' result
      go (QZ qry) (Z (Pair (SingleEraLedgerConfig cfg) st)) =
          Right $ answerQuery cfg qry (currentLedgerState st)
      go (QS qry) (S st) =
          first shiftMismatch $ go qry st
      go (QZ qry) (S st) =
          Left $ MismatchEraInfo $ ML (queryInfo qry) (hcmap proxySingle ledgerInfo st)
      go (QS qry) (Z st) =
          Left $ MismatchEraInfo $ MR (hardForkQueryInfo qry) (ledgerInfo st)

  eqQuery = \(HardForkQuery qry) (HardForkQuery qry') ->
      -- Lift the type equality to the @Either@
      case go qry qry' of
        Nothing   -> Nothing
        Just Refl -> Just Refl
    where
      go :: All SingleEraBlock xs'
         => HardForkQuery xs' result
         -> HardForkQuery xs' result'
         -> Maybe (result :~: result')
      go (QZ qry) (QZ qry') = eqQuery qry qry'
      go (QS qry) (QS qry') = go qry qry'
      go _        _         = Nothing

deriving instance CanHardFork xs => Show (Query (HardForkBlock xs) result)

getHardForkQuery :: Query (HardForkBlock xs) (HardForkQueryResult xs result)
                 -> HardForkQuery xs result
getHardForkQuery (HardForkQuery qry) = qry

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

data HardForkQuery :: [*] -> * -> * where
  QZ :: Query x result          -> HardForkQuery (x ': xs) result
  QS :: HardForkQuery xs result -> HardForkQuery (x ': xs) result

deriving instance All SingleEraBlock xs => Show (HardForkQuery xs result)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo :: forall blk. SingleEraBlock blk
           => Product SingleEraLedgerConfig CurrentLedgerState blk
           -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

queryInfo :: forall blk result. SingleEraBlock blk
          => Query blk result -> SingleEraInfo blk
queryInfo _ = singleEraInfo (Proxy @blk)

hardForkQueryInfo :: All SingleEraBlock xs
                  => HardForkQuery xs result -> NS SingleEraInfo xs
hardForkQueryInfo = go
  where
    go :: All SingleEraBlock xs'
       => HardForkQuery xs' result -> NS SingleEraInfo xs'
    go (QZ qry) = Z (queryInfo qry)
    go (QS qry) = S (go qry)

shiftMismatch :: MismatchEraInfo xs -> MismatchEraInfo (x ': xs)
shiftMismatch = MismatchEraInfo . MS . getMismatchEraInfo

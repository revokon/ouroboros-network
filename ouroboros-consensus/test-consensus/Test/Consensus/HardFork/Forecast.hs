{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}

module Test.Consensus.HardFork.Forecast (tests) where

import           Control.Monad.Except
import           Data.Either (isRight)
import           Data.Foldable (toList)
import           Data.List (inits)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Word
import           GHC.Stack

import           Test.QuickCheck hiding (elements)
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (elements)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..), Summary (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HardFork.History.Util
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly, splits)

import           Test.Consensus.HardFork.Infra
import           Test.Util.QuickCheck

tests :: TestTree
tests = testGroup "Forecast" [
      testGroup "Sanity" [
          testProperty "generator" $ checkGenerator prop_validTestSetup
        , testProperty "shrinker"  $ checkShrinker  prop_validTestSetup
        , testProperty "forecastWithinEra" $ prop_forecastWithinEra
        ]
    ]

{-
mkHardForkForecast :: InPairs (Translate f) xs
                   -> Telescope (Past g) (Current (AnnForecast f)) xs
                   -> Forecast (HardForkLedgerView_ f xs)
-}

{-------------------------------------------------------------------------------
  Mock chain and ledger
-------------------------------------------------------------------------------}

newtype Chain era = Chain { getBlocks :: [Block] }
  deriving (Show)

data Block = Block SlotNo Scheduled
  deriving (Show)

data LedgerUpdate = IncreaseValueBy Word64
  deriving (Show, Eq)

type Scheduled = Map SlotNo LedgerUpdate

{-------------------------------------------------------------------------------
  Ledger state
-------------------------------------------------------------------------------}

type LedgerValue = Word64

data LedgerState = LedgerState {
      ledgerValue     :: LedgerValue
    , ledgerScheduled :: Scheduled
    , ledgerTip       :: WithOrigin SlotNo
    }
  deriving (Show)

data instance Ticked LedgerState = TickedLedgerState {
      tickedValue     :: LedgerValue
    , tickedScheduled :: Scheduled
    }
  deriving (Show)

initLedgerState :: LedgerState
initLedgerState = LedgerState {
      ledgerValue     = 0
    , ledgerScheduled = Map.empty
    , ledgerTip       = Origin
    }

tickLedgerState :: SlotNo -> LedgerState -> Ticked LedgerState
tickLedgerState curSlot LedgerState{..} =
    -- Apply all updates in order, until we reach the new slot
    repeatedly updateTicked slots initTicked
  where
    slots :: [SlotNo]
    slots = case ledgerTip of
              Origin      -> [SlotNo 0 .. curSlot]
              NotOrigin s -> [succ s   .. curSlot]

    initTicked :: Ticked LedgerState
    initTicked = TickedLedgerState {
          tickedScheduled = ledgerScheduled
        , tickedValue     = ledgerValue
        }

    updateTicked :: SlotNo -> Ticked LedgerState -> Ticked LedgerState
    updateTicked slot TickedLedgerState{..} = TickedLedgerState {
          tickedScheduled = Map.delete slot tickedScheduled
        , tickedValue     = case Map.lookup slot tickedScheduled of
                              Nothing                  -> tickedValue
                              Just (IncreaseValueBy d) -> tickedValue + d
        }

-- A value of @x@ in era @n@ corresponds to a value of @2x@ in era @n+1@
--
-- This means that the HFC translation functions have some work to do.
doubleLedgerValues :: LedgerState -> LedgerState
doubleLedgerValues LedgerState{..} = LedgerState{
      ledgerTip       = ledgerTip
    , ledgerValue     = applyEraTransition ledgerValue
    , ledgerScheduled = ledgerScheduled
    }
  where
    applyEraTransition :: LedgerValue -> LedgerValue
    applyEraTransition = (*3)

-- | Advance ledger state to the next slot (without a block)
stepLedgerState :: LedgerState -> LedgerState
stepLedgerState ledgerState = LedgerState {
      ledgerScheduled = tickedScheduled
    , ledgerValue     = tickedValue
    , ledgerTip       = NotOrigin nextSlot
    }
  where
    nextSlot :: SlotNo
    nextSlot = case ledgerTip ledgerState of
                 Origin      -> SlotNo 0
                 NotOrigin s -> succ s

    TickedLedgerState{..} = tickLedgerState nextSlot ledgerState

applyBlock :: Block -> Ticked LedgerState -> LedgerState
applyBlock (Block slot blockScheduled) TickedLedgerState{..} = LedgerState {
      ledgerScheduled = Map.unionWith combineUpdates
                          tickedScheduled
                          blockScheduled
    , ledgerValue     = tickedValue
    , ledgerTip       = NotOrigin slot
    }
  where
    combineUpdates :: LedgerUpdate -> LedgerUpdate -> LedgerUpdate
    combineUpdates (IncreaseValueBy x) (IncreaseValueBy y) =
        IncreaseValueBy (x + y)

{-------------------------------------------------------------------------------
  Forecasting within an era
-------------------------------------------------------------------------------}

withinEraForecast :: MaxLookahead -> LedgerState -> Forecast LedgerState
withinEraForecast maxLookAhead st = Forecast{
      forecastAt  = ledgerTip st
    , forecastFor = go
    }
  where
    go :: SlotNo -> Except OutsideForecastRange (Ticked LedgerState)
    go for = do
        when (for >= upperBound) $
          throwError OutsideForecastRange {
                  outsideForecastAt     = ledgerTip st
                , outsideForecastMaxFor = upperBound
                , outsideForecastFor    = for
                }

        return $ tickLedgerState for st
      where
        -- Exclusive upper bound
        upperBound :: SlotNo
        upperBound = case ledgerTip st of
                       Origin      -> addSlots maxLookAhead (SlotNo 0)
                       NotOrigin s -> addSlots maxLookAhead (succ s)

{-------------------------------------------------------------------------------
  Forecast validity
-------------------------------------------------------------------------------}

correctForecastOf :: Ticked LedgerState -> LedgerState -> Property
forecast `correctForecastOf` actual =
      counterexample ("forecast: " ++ show forecast)
    $ counterexample ("actual: " ++ show actual)
    $ conjoin [
          tickedValue forecast === ledgerValue actual
        , isSubmapOfBy isSubUpdate (tickedScheduled forecast) (ledgerScheduled actual)
        ]
  where
    -- It is possible that later blocks schedule another delta for the same
    -- slot, but this can only _increase_ the total update to the ledger
    isSubUpdate :: LedgerUpdate -> LedgerUpdate -> Property
    isSubUpdate (IncreaseValueBy a) (IncreaseValueBy b) = b `ge` a

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

prop_validTestSetup :: TestSetup -> Property
prop_validTestSetup setup@TestSetup{..} = conjoin [
        counterexample "strictlyIncreasing" $
          strictlyIncreasing $ map (\(Block s _) -> s) $ concat $ blocksPerEra setup
      , counterexample "obeysMaxLookahead" $
          conjoin $ map checkLookahead testEras
      , counterexample "validForecastParams" $
          validForecastParams setup === Right ()
      ]
  where
    checkLookahead :: TestEra -> Property
    checkLookahead TestEra{..} = conjoin [
          slotChange `ge` addSlots testEraMaxLookahead slotBlock
        | (Block slotBlock scheduled) <- testEraBlocks
        , (slotChange, _update) <- Map.toList scheduled
        ]

prop_forecastWithinEra :: TestSetup -> Property
prop_forecastWithinEra setup@TestSetup{..} =
      tabulate "within range" [show $ isRight mForecastLedger]
    $ counterexample ("ledgerStates: " ++ show ledgerStates)
    $ counterexample ("markTransitions: " ++ show (markTransitions setup))
    $ case mForecastLedger of
        Left _outOfRange ->
          property True
        Right forecastLedger ->
          forecastLedger `correctForecastOf` actualLedger
  where
    TestForecastParams{..} = testForecastParams

    ledgerStates :: Map (WithOrigin SlotNo) LedgerState
    ledgerStates = interpretChain setup

    ledgerStateAt :: HasCallStack => WithOrigin SlotNo -> LedgerState
    ledgerStateAt mSlotNo =
        Map.findWithDefault
          (error $ concat [
               "ledgerStateAt: no ledger state for "
             , show mSlotNo
             , " in "
             , show ledgerStates
             ])
          mSlotNo
          ledgerStates

    forecast :: Forecast LedgerState
    forecast = withinEraForecast
                 (slotMaxLookahead setup testForecastAt)
                 (ledgerStateAt testForecastAt)

    mForecastLedger :: Either OutsideForecastRange (Ticked LedgerState)
    mForecastLedger = runExcept $ forecastFor forecast testForecastWithinEra

    actualLedger :: LedgerState
    actualLedger = ledgerStateAt (NotOrigin testForecastWithinEra)

{-------------------------------------------------------------------------------
  Valued derived from the 'TestSetup'
-------------------------------------------------------------------------------}

-- | Mark era transitions
--
-- This is an auxiliary type used in 'interpretChain'. It records the start of
-- end of the current era (equals start of the next)
data EraTransition = EraTransition SlotNo
  deriving (Show)

markTransitions :: TestSetup -> [Either Block EraTransition]
markTransitions =
    concatMap (either (map Left) ((:[]) . Right)) . go . testEras
  where
    go :: [TestEra] -> [Either [Block] EraTransition]
    go []        = []
    go [e]       = [Left (testEraBlocks e)]
    go (e:e':es) = Left (testEraBlocks e)
                 : Right (EraTransition (boundSlot (eraStart (testEraSummary e'))))
                 : go (e' : es)

-- | The ledger state at every 'SlotNo'
interpretChain :: TestSetup -> Map (WithOrigin SlotNo) LedgerState
interpretChain setup@TestSetup{..} =
    Map.fromList $
        (Origin, initLedgerState)
      : go startSlot initLedgerState (markTransitions setup)
  where
    -- The 'endSlot' is the max 'SlotNo' we might need a ledger state for
    startSlot, endSlot :: SlotNo
    startSlot = SlotNo 0
    endSlot   = testForecastWithinEra testForecastParams

    go :: SlotNo       -- Next expected slot
       -> LedgerState  -- Previous state
       -> [Either Block EraTransition]
       -> [(WithOrigin SlotNo, LedgerState)]
    go curSlot prevLedger [] =
        pad curSlot prevLedger
    go curSlot prevLedger xs@(Left blk@(Block s _):xs')
      | s > curSlot = (NotOrigin curSlot, stepped) : go (succ curSlot) stepped xs
      | otherwise   = (NotOrigin curSlot, applied) : go (succ curSlot) applied xs'
      where
        stepped = stepLedgerState prevLedger
        ticked  = tickLedgerState curSlot prevLedger
        applied = applyBlock blk ticked
    -- Applying the transition itself does not advance the slot
    -- (there might be a block in the very first slot in the next era)
    go curSlot prevLedger xs@(Right (EraTransition s):xs')
      | s > curSlot = (NotOrigin curSlot, stepped) : go (succ curSlot) stepped xs
      | otherwise   =                                go       curSlot  doubled xs'
      where
        stepped = stepLedgerState    prevLedger
        doubled = doubleLedgerValues prevLedger

    -- After we have applied the final block, keep ticking the ledger state
    -- until we have reached the required 'SlotNo'
    pad :: SlotNo -> LedgerState -> [(WithOrigin SlotNo, LedgerState)]
    pad curSlot prevLedger
      | curSlot > endSlot = []
      | otherwise         = (NotOrigin curSlot, stepped) : pad (succ curSlot) stepped
      where
        stepped = stepLedgerState prevLedger

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

data TestEra = TestEra {
      -- | Era summary (the 'EraParams' and bounds)
      --
      -- NOTE: The 'EraParams' (including associated safe zone) are independent
      -- from the lookahead, which is a property of the ledger ("how far into
      -- the future can we look and still know the ledger state"). The safe
      -- zones of the 'EraParams' only provide guarantees about when we can
      -- expect era transitions.
      testEraSummary      :: EraSummary

      -- | The maximum look ahead
      --
      -- The HFC itself does not impose any restrictions on the relation between
      -- the max lookahead of various eras. If the max lookahead in era B is
      -- smaller than the max lookahead in era A, this " merely " poses a
      -- problem for the translation function.
    , testEraMaxLookahead :: MaxLookahead

      -- | Blocks on the chain in this era
      --
      -- NOTE: Nothing in the chain or the ledger depends on epoch boundaries.
    , testEraBlocks       :: [Block]
    }
  deriving (Show)

-- | The parameters for the forecast we construct
--
-- The forecast is constructed in a single era. The HFC combinator is
-- responsible for extending it across eras (that's precisely what we're
-- testing in this module, of course).
data TestForecastParams = TestForecastParams {
      -- | Anchor of the forecast
      testForecastAt        :: WithOrigin SlotNo

      -- | An arbitrary slot number within the forecast's era
      --
      -- This is used as a sanity check to make sure that within-era
      -- forecasting works as expected.
      --
      -- Must be at or after 'testForecastAt'.
    , testForecastWithinEra :: SlotNo
    }
  deriving (Show)

data TestSetup = TestSetup {
      testEras           :: [TestEra]
    , testForecastParams :: TestForecastParams
    }
  deriving (Show)

type MaxLookahead = Word64

{-------------------------------------------------------------------------------
  Invariant
-------------------------------------------------------------------------------}

validForecastParams :: TestSetup -> Either String ()
validForecastParams setup@TestSetup{..} = runExcept $ do
    era <- case slotEra' setup testForecastAt of
             Just era -> return era
             Nothing  -> throwError $ mconcat [
                 "No era known for 'testForecastAt' == "
               , show testForecastAt
               ]

    unless (testEraContains (NotOrigin testForecastWithinEra) era) $
      throwError $ mconcat [
          "'testForecastWithinEra' == "
        , show testForecastWithinEra
        , " not in same era as 'testForecastAt' == "
        , show testForecastAt
        ]
  where
    TestForecastParams{..} = testForecastParams

{-------------------------------------------------------------------------------
  Query 'TestEra'
-------------------------------------------------------------------------------}

testEraContains :: WithOrigin SlotNo -> TestEra -> Bool
testEraContains mSlot TestEra{..} = and [
      boundSlot eraStart <= fromWithOrigin (SlotNo 0) mSlot
    , case (mSlot, eraEnd) of
        (NotOrigin s, EraEnd end) -> s < boundSlot end
        _otherwise                -> True
    ]
  where
    EraSummary{..} = testEraSummary

testEraNext :: TestEra -> Maybe EpochNo
testEraNext TestEra{..} =
    case eraEnd testEraSummary of
      EraEnd end   -> Just $ boundEpoch end
      EraUnbounded -> Nothing

{-------------------------------------------------------------------------------
  Query the 'TestSetup'
-------------------------------------------------------------------------------}

blocksPerEra :: TestSetup -> [[Block]]
blocksPerEra = map testEraBlocks . testEras

-- | Era containing the given slot, if any
slotEra' :: TestSetup -> WithOrigin SlotNo -> Maybe TestEra
slotEra' TestSetup{..} mSlot =
    listToMaybe $ filter (testEraContains mSlot) testEras

-- | Wrapper around 'slotEra' to be used when the era should exist
slotEra :: HasCallStack => TestSetup -> WithOrigin SlotNo -> TestEra
slotEra setup mSlot =
    case slotEra' setup mSlot of
      Nothing  -> error $ "slotEra: unknown slot " ++ show mSlot
      Just era -> era

-- | Era parameters of the era containing the slot
slotEraParams :: TestSetup -> WithOrigin SlotNo -> EraParams
slotEraParams setup = eraParams . testEraSummary . slotEra setup

-- | Transition to the next era from the era containing the slot (if any)
slotEraNext :: TestSetup -> WithOrigin SlotNo -> Maybe EpochNo
slotEraNext setup = testEraNext . slotEra setup

-- | Maximum lookahead of the ledger in the era containing the slot
slotMaxLookahead :: TestSetup -> WithOrigin SlotNo -> MaxLookahead
slotMaxLookahead setup = testEraMaxLookahead . slotEra setup

-- | Check if two slots are in the same era
slotSameEra :: TestSetup -> WithOrigin SlotNo -> WithOrigin SlotNo -> Bool
slotSameEra setup otherSlot = testEraContains otherSlot . slotEra setup

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

instance Arbitrary TestSetup where
  arbitrary = chooseEras $ \ixs -> do
      summary  <- toList . getSummary <$> genSummary ixs
      eras     <- mapM genTestEra summary
      forecast <- genForecastParams eras

      return TestSetup{
          testEras           = eras
        , testForecastParams = forecast
        }
    where
      genTestEra :: EraSummary -> Gen TestEra
      genTestEra summary@EraSummary{..} = sized' $ \sz -> do
          maxLookahead <- choose (0, sz)
          upperBound   <- case eraEnd of
            EraEnd bound -> return bound
            EraUnbounded -> mkUpperBound eraParams eraStart <$> choose (0, sz)
          mBlocks <- forM (enumIncExc (boundSlot eraStart) (boundSlot upperBound)) $ \slot -> do
            slotFilled <- arbitrary
            if slotFilled then do
              scheduled <- genScheduled maxLookahead slot
              return $ Just (Block slot scheduled)
            else
              return Nothing
          return TestEra {
              testEraSummary      = summary
            , testEraMaxLookahead = maxLookahead
            , testEraBlocks       = catMaybes mBlocks
            }

      genScheduled :: MaxLookahead -> SlotNo -> Gen Scheduled
      genScheduled maxLookahead slotBlock = do
          numChanges <- choose (0, 2)
          fmap Map.fromList $
            replicateM numChanges $ genChange maxLookahead slotBlock

      genChange :: MaxLookahead -> SlotNo -> Gen (SlotNo, LedgerUpdate)
      genChange maxLookahead slotBlock = sized' $ \sz -> do
          skip     <- choose (0, sz)
          increase <- choose (0, 2)
          -- If the maxLookahead is zero (no look ahead possible), the change
          -- is applied when we apply the block (i.e., in the same slot).
          let slotChange = addSlots (maxLookahead + skip) slotBlock
          return (slotChange, IncreaseValueBy increase)

      -- Construct an upper bound for an era, given number of epochs
      mkUpperBound :: EraParams -> Bound -> Word64 -> Bound
      mkUpperBound eraParams eraStart =
            History.mkUpperBound eraParams eraStart
          . flip addEpochs (boundEpoch eraStart)

      genForecastParams :: [TestEra] -> Gen TestForecastParams
      genForecastParams eras = sized' $ \sz -> do
          -- Pick an era for the forecast
          (isFirstEra, TestEra{..}) <- elements $ zip (True : repeat False) eras
          let EraSummary{..} = testEraSummary

          -- Pick an anchor
          at <- oneof $ concat [
              [ fmap NotOrigin $ elements $
                  enumIncExc
                    (boundSlot eraStart)
                    (boundSlot end)
              | EraEnd end <- [eraEnd]
              ]

            , [ do upperBound <- choose (1, 1 + sz) -- upper bound is exclusive
                   fmap NotOrigin $ elements $
                     enumIncExc
                       (boundSlot eraStart)
                       (addSlots upperBound (boundSlot eraStart))
              | EraUnbounded <- [eraEnd]
              ]

            , [ return Origin
              | isFirstEra
              ]
            ]

          -- Pick a slot within the same era
          -- (for within-era forecast sanity check)
          let at' = fromWithOrigin (SlotNo 0) at
          withinEra <- oneof $ concat [
              [ elements $ enumIncExc at' (boundSlot end)
              | EraEnd end <- [eraEnd]
              ]

            , [ do upperBound <- choose (1, 1 + sz) -- upper bound is exclusive
                   elements $ enumIncExc at' (addSlots upperBound at')
              | EraUnbounded <- [eraEnd]
              ]
            ]

          return TestForecastParams {
              testForecastAt        = at
            , testForecastWithinEra = withinEra
            }


  shrink setup@TestSetup{..} = concat [
        -- Shrink the eras
        [ setup'
        | eras' <- shrinkEras testEras
        , let setup' = setup { testEras = eras' }
          -- The forecast parameters might not be valid anymore:
          -- * The era of the forecast anchor might not exist anymore
          -- * Due to reducing the bounds of an era, the within-era 'at'
          --   might not actually be within-era anymore
          -- * Due to a reduction in the max lookahead, a forecast might
          --   now exceed the maximum.
        , validForecastParams setup' == Right ()
        ]

        -- Shrink the forecast params
      , [ setup'
        | params' <- shrinkForecastParams testForecastParams
        , let setup' = setup { testForecastParams = params' }
          -- The forecast parameters might not be valid anymore:
          -- * By shrinking the anchor of the forecast, it might not be
          --   in the same era as the within-era 'at' anymore
        , validForecastParams setup' == Right ()
        ]
      ]
    where
      shrinkEras :: [TestEra] -> [[TestEra]]
      shrinkEras eras = concat [
            -- Drop some eras (from the end)
            [ eras'
            | eras' <- init (inits eras)
            ]

            -- Shift the era bounds
          , shiftBounds eras

            -- Shrink one era
          , [ xs ++ [y'] ++ zs
            | (xs, y, zs) <- splits eras
            , y' <- shrinkEra y
            ]
          ]

      shrinkEra :: TestEra -> [TestEra]
      shrinkEra era@TestEra{..} = concat [
            -- Drop some blocks
            [ era'
            | blocks' <- shrinkList (const []) testEraBlocks
            , let era' = era { testEraBlocks = blocks' }
            ]

            -- | Shrink blocks
            --
            -- We don't use shrinkList for this, because we need some context
          , [ era'
            | (xs, y, zs) <- splits testEraBlocks
            , let prev | null xs   = Nothing
                       | otherwise = Just (last xs)
            , y' <- shrinkBlock testEraSummary testEraMaxLookahead prev y
            , let era' = era { testEraBlocks = xs ++ [y'] ++ zs }
            ]

            -- Shrink the max look-ahead (if possible)
          , [ era'
            | maxLookahead' <- shrink testEraMaxLookahead
            , let era' = era { testEraMaxLookahead = maxLookahead' }
            ]
          ]

      shrinkBlock :: EraSummary -> MaxLookahead -> Maybe Block -> Block -> [Block]
      shrinkBlock summary maxLookahead mPrev (Block (SlotNo slot) scheduled) = concat [
            -- Move the block earlier into the era
            --
            -- NOTE: Moving a block _earlier_ into the chain can't violate
            -- the max-lookahead, as the distance between the block and the
            -- change can only _increase_
            [ Block slot' scheduled
            | slot' <- map SlotNo $ shrink slot
              -- Don't clash with the previous block
            , case mPrev of
                Just (Block prevSlot _) -> slot' > prevSlot
                Nothing                 -> True
              -- Don't move block out of this era
            , slot' >= boundSlot (eraStart summary)
            ]

            -- Shrink the block body
          , [ Block (SlotNo slot) scheduled'
            | scheduled' <- shrinkScheduled maxLookahead (SlotNo slot) scheduled
            ]
          ]

      shrinkScheduled :: MaxLookahead -> SlotNo -> Scheduled -> [Scheduled]
      shrinkScheduled maxLookahead slotBlock =
            map Map.fromList
          . shrinkList shrinkUpdate
          . Map.toList
        where
          shrinkUpdate :: (SlotNo, LedgerUpdate) -> [(SlotNo, LedgerUpdate)]
          shrinkUpdate (SlotNo slotUpdate, update@(IncreaseValueBy newLedgerValue)) = concat [
                -- Shrink the ledger value (complicated ledger values distract)
                [ (SlotNo slotUpdate, IncreaseValueBy newLedgerValue')
                | newLedgerValue' <- shrink newLedgerValue
                ]

                -- Try to do the update sooner
              , [ (slotUpdate', update)
                | slotUpdate' <- map SlotNo $ shrink slotUpdate
                  -- The earliest it can change is the very next slot
                , slotUpdate' > slotBlock
                  -- We must still obey the maxLookahead though
                , countSlots slotUpdate' slotBlock > maxLookahead
                ]
              ]

      shrinkForecastParams :: TestForecastParams -> [TestForecastParams]
      shrinkForecastParams params@TestForecastParams{..} = concat [
            [ params'
            | at' <- shrinkSlotNo' testForecastAt
            , let params' = params { testForecastAt = at' }
            ]

          , [ params'
            | withinEra' <- shrinkSlotNo testForecastWithinEra
            , NotOrigin withinEra' >= testForecastAt
            , let params' = params { testForecastWithinEra = withinEra' }
            ]
          ]

      shrinkSlotNo' :: WithOrigin SlotNo -> [WithOrigin SlotNo]
      shrinkSlotNo' Origin        = []
      shrinkSlotNo' (NotOrigin s) = Origin : map NotOrigin (shrinkSlotNo s)

      shrinkSlotNo :: SlotNo -> [SlotNo]
      shrinkSlotNo (SlotNo s) = map SlotNo (shrink s)

      shiftBounds :: [TestEra] -> [[TestEra]]
      shiftBounds []  =
          error "shiftBounds: empty list"
      shiftBounds [e@TestEra{..}] =
          case eraEnd testEraSummary of
            EraUnbounded -> []
            EraEnd end   -> [
                [e {testEraSummary = testEraSummary {eraEnd = end'}}]
              | end' <- EraUnbounded
                      : map EraEnd (shiftEnd testEraSummary testEraBlocks end)
              ]
      shiftBounds (e:e':es) = concat [
            -- Shift the bound between e and e'
            -- (The new end of e will be the new start of e')
            [   e  {testEraSummary = (testEraSummary e)  {eraEnd   = EraEnd end'}}
              : e' {testEraSummary = (testEraSummary e') {eraStart = end'       }}
              : es
            | EraEnd end <- [eraEnd (testEraSummary e)]
            , end' <- shiftEnd (testEraSummary e) (testEraBlocks e) end
            ]

            -- Shift the other bounds
          , [ e:es'
            | es' <- shiftBounds (e':es)
            ]
          ]

      shiftEnd :: EraSummary -> [Block] -> Bound -> [Bound]
      shiftEnd EraSummary{..} blocks Bound{boundEpoch = EpochNo epoch} = [
            end'
          | epoch' <- map EpochNo $ shrink epoch
          , epoch' > boundEpoch eraStart
          , let end' = History.mkUpperBound eraParams eraStart epoch'
            -- Make sure all of the blocks are still within the era
          , if null blocks then
              True
            else
              let Block s _ = last blocks
              in s < boundSlot end'
          ]

{-------------------------------------------------------------------------------
  Interpreting the chain
-------------------------------------------------------------------------------}


-- withinEraForecast' :: Telescope (Past (K Void)) (Current (AnnForecast LedgerState)) xs

-- acrossEraForecast :: Forecast


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Like 'enumFromTo', but with an exclusive upper bound
enumIncExc :: forall a. (Ord a, Enum a) => a -> a -> [a]
enumIncExc lo hi = go lo
  where
    go :: a -> [a]
    go x | x >= hi   = []
         | otherwise = x : go (succ x)

sized' :: (Word64 -> Gen a) -> Gen a
sized' f = sized (f . fromIntegral)

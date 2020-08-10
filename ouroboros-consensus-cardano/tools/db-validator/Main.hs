{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Database validation tool. Also works as an analysis tool if an
-- 'AnalysisName' flag is enabled.
module Main (main) where

import           Data.Foldable (asum)
import           Options.Applicative

import           Control.Tracer (contramap, debugTracer, nullTracer)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import qualified Ouroboros.Consensus.Node as Node
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Util.IOLike (atomically)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry

import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.ChainDB.Impl.Args (fromChainDbArgs)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.ImmDB as ImmDB

import           Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..))

import           Ouroboros.Consensus.Shelley.Node (Nonce (..))

import           Analysis
import           Class

main :: IO ()
main = do
    cmdLine <- getCmdLine
    warnUnusedFlags cmdLine
    case blockType cmdLine of
      ByronBlock   args -> validateOrAnalyse cmdLine args
      ShelleyBlock args -> validateOrAnalyse cmdLine args
      CardanoBlock args -> validateOrAnalyse cmdLine args

warnUnusedFlags :: CmdLine -> IO ()
warnUnusedFlags CmdLine{..} = case (analysis, onlyImmDB) of
  (Just _, True)
    -> putStrLn "Warning: Flag onlyImmDB is ignored on analysis mode"
  _ -> return ()

data CmdLine = CmdLine {
    dbDir     :: FilePath
  , verbose   :: Bool
  , onlyImmDB :: Bool
  , blockType :: BlockType
  , analysis  :: Maybe AnalysisName
  }

data BlockType =
    ByronBlock   ByronBlockArgs
  | ShelleyBlock ShelleyBlockArgs
  | CardanoBlock CardanoBlockArgs

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser CmdLine
parseCmdLine = CmdLine
    <$> strOption (mconcat [
            long "db"
          , help "Path to the chain DB"
          , metavar "PATH"
          ])
    <*> flag False True (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> flag False True (mconcat [
            long "onlyImmDB"
          , help "Validate only the immutable DB (e.g. do not do ledger validation)"
          ])
    <*> blockTypeParser
    <*> parseAnalysis

parseAnalysis :: Parser (Maybe AnalysisName)
parseAnalysis = parseMaybe $ asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    , flag' ShowEBBs $ mconcat [
          long "show-ebbs"
        , help "Show all EBBs and their predecessors"
        ]
    ]

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "byron"   (info (parseByronType   <**> helper) (progDesc "byron command"  ))
  , command "shelley" (info (parseShelleyType <**> helper) (progDesc "shelley command"))
  , command "cardano" (info (parseCardanoType <**> helper) (progDesc "cardano command"))
  ]

parseByronType :: Parser BlockType
parseByronType = ByronBlock <$> parseByronArgs

parseByronArgs :: Parser ByronBlockArgs
parseByronArgs = ByronBlockArgs
    <$> strOption (mconcat [
            long "configByron"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> flag False True (mconcat [
            long "testnet"
          , help "The DB contains blocks from testnet rather than mainnet"
          ])
    <*> parseMaybe (option auto (mconcat [
            long "genesisHash"
          , help "Expected genesis hash"
          , metavar "HASH"
          ]))
    <*> parseMaybe (PBftSignatureThreshold <$> thresholdParser)
  where
    thresholdParser = option auto (mconcat [
            long "threshold"
          , help "PBftSignatureThreshold"
          , metavar "THRESHOLD"
          ])

parseShelleyType :: Parser BlockType
parseShelleyType = ShelleyBlock <$> parseShelleyArgs

parseShelleyArgs :: Parser ShelleyBlockArgs
parseShelleyArgs = ShelleyBlockArgs
    <$> strOption (mconcat [
            long "configShelley"
          , help "Path to config file."
          , metavar "PATH"
          ])
    <*> asum [ Nonce  <$> parseNonce
             , pure NeutralNonce]
  where
    parseNonce = strOption (mconcat [
            long "nonce"
          , help "initial nonce"
          , metavar "NONCE"
          ])

parseCardanoType :: Parser BlockType
parseCardanoType = CardanoBlock <$> parseCardanoArgs

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs = CardanoBlockArgs
    <$> parseByronArgs
    <*> parseShelleyArgs

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

getCmdLine :: IO CmdLine
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to validate a Chain DB"
        ])

{-------------------------------------------------------------------------------
  Valitate or analyse
-------------------------------------------------------------------------------}

validateOrAnalyse :: forall blk.
                     (HasAnalysis blk, Node.RunNode blk, Show (Header blk))
                  => CmdLine -> Args blk -> IO ()
validateOrAnalyse cmd@CmdLine {..} args = do
    protocolInfo <- mkProtocolInfo args
    case analysis of
      Nothing           -> validate dbDir protocolInfo onlyImmDB verbose
      Just analysisName -> analyse cmd analysisName protocolInfo

analyse :: forall blk. (RunNode blk, HasAnalysis blk)
        => CmdLine -> AnalysisName -> ProtocolInfo IO blk -> IO ()
analyse CmdLine{..} analysisName protocolInfo =
    withRegistry $ \registry ->
      Analysis.withImmDB dbDir cfg chunkInfo registry verbose $ \immDB -> do
        runAnalysis analysisName cfg immDB registry
        putStrLn "Done"
  where
    cfg       = pInfoConfig protocolInfo
    chunkInfo = nodeImmDbChunkInfo cfg

validate
  :: (Node.RunNode blk, Show (Header blk))
  => FilePath -- ^ DB directory
  -> ProtocolInfo IO blk
  -> Bool -- Immutable DB only?
  -> Bool -- Verbose
  -> IO ()
validate dbDir protocolInfo onlyImmDB verbose =
    withRegistry $ \registry -> do
      let chainDbArgs = mkChainDbArgs registry InFuture.dontCheck
          (immDbArgs, _, _, _) = fromChainDbArgs chainDbArgs
      if onlyImmDB then
        ImmDB.withImmDB immDbArgs $ \immDB -> do
          immDbTipPoint <- ImmDB.getPointAtTip immDB
          putStrLn $ "DB tip: " ++ show immDbTipPoint
      else
        ChainDB.withDB chainDbArgs $ \chainDB -> do
          chainDbTipPoint <- atomically $ ChainDB.getTipPoint chainDB
          putStrLn $ "DB tip: " ++ show chainDbTipPoint
  where
    ProtocolInfo { pInfoInitLedger = initLedger, pInfoConfig = cfg } =
      protocolInfo

    tracer
      | verbose   = contramap show debugTracer
      | otherwise = nullTracer

    chunkInfo  = Node.nodeImmDbChunkInfo cfg

    mkChainDbArgs registry btime =
      let args = Node.mkChainDbArgs tracer registry btime
                   dbDir cfg initLedger chunkInfo
      in args {
          ChainDB.cdbImmValidation = ImmDB.ValidateAllChunks
        }

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Node.Types where

import           Cardano.BM.Data.Tracer         (ToObject (..))
import           Cardano.BM.Data.Tracer.Extras  (Tagged (..), mkObjectStr)
import           Control.Lens                   (Iso', iso, makeLenses, view)
import           Control.Monad.Freer.Log        (LogMessage)
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text.Prettyprint.Doc      (Pretty (..), pretty, (<+>))
import           Data.Time.Units                (Second)
import           Data.Time.Units.Extra          ()
import           GHC.Generics                   (Generic)
import qualified Language.Plutus.Contract.Trace as Trace
import           Ledger                         (Slot, Tx, txId)
import           Servant                        (FromHttpApiData, ToHttpApiData)
import           Servant.Client                 (BaseUrl)
import           Wallet.Emulator                (Wallet)
import qualified Wallet.Emulator                as EM
import           Wallet.Emulator.Chain          (ChainEvent, ChainState)
import qualified Wallet.Emulator.MultiAgent     as MultiAgent

data BlockReaperConfig =
    BlockReaperConfig
        { brcInterval     :: Second
        , brcBlocksToKeep :: Int
        }
    deriving (Show, Eq, Generic, FromJSON)


data GenRandomTxMsg = GeneratingRandomTransaction

instance Pretty GenRandomTxMsg where
    pretty GeneratingRandomTransaction = "Generating a random transaction"

data NodeFollowerLogMsg =
    NewFollowerId FollowerID
    | GetBlocksFor FollowerID
    | LastBlock Int
    | NewLastBlock Int
    | GetCurrentSlot Slot

instance Pretty NodeFollowerLogMsg where
    pretty  = \case
        NewFollowerId newID -> "New follower ID:" <+> pretty newID
        GetBlocksFor i      -> "Get blocks for" <+> pretty i
        LastBlock i         -> "Last block:" <+> pretty i
        NewLastBlock i      -> "New last block:" <+> pretty i
        GetCurrentSlot s    -> "Get current slot:" <+> pretty s

data NodeServerMsg =
    NodeServerFollowerMsg NodeFollowerLogMsg
    | NodeGenRandomTxMsg GenRandomTxMsg
    | NodeMockNodeMsg MockNodeLogMsg


instance Pretty NodeServerMsg where
    pretty = \case
        NodeServerFollowerMsg m -> pretty m
        NodeGenRandomTxMsg m    -> pretty m
        NodeMockNodeMsg m       -> pretty m

data MockServerConfig =
    MockServerConfig
        { mscBaseUrl          :: BaseUrl
        , mscSlotLength       :: Second
        -- ^ Duration of one slot
        , mscRandomTxInterval :: Maybe Second
        -- ^ Time between two randomly generated transactions
        , mscBlockReaper      :: Maybe BlockReaperConfig
        -- ^ When to discard old blocks
        , mscInitialTxWallets :: [Wallet]
        -- ^ The wallets that receive money from the initial transaction.
        , mscSocketPath       :: FilePath
        -- ^ Path to the socket used to communicate with the server.
        }
    deriving (Show, Eq, Generic, FromJSON)

data AppState =
    AppState
        { _chainState    :: ChainState
        , _eventHistory  :: [LogMessage ChainEvent]
        , _followerState :: NodeFollowerState
        }
    deriving (Show)


data MockNodeLogMsg =
        AddingSlot
        | AddingTx Tx

instance Pretty MockNodeLogMsg where
    pretty AddingSlot   = "Adding slot"
    pretty (AddingTx t) = "AddingTx" <+> pretty (Ledger.txId t)

initialChainState :: Trace.InitialDistribution -> ChainState
initialChainState =
    view EM.chainState .
    MultiAgent.emulatorStateInitialDist . Map.mapKeys EM.walletPubKey

-- | 'AppState' with an initial transaction that pays some Ada to
--   the wallets.
initialAppState :: [Wallet] -> AppState
initialAppState wallets =
    AppState
        { _chainState = initialChainState (Trace.defaultDistFor wallets)
        , _eventHistory = mempty
        , _followerState = initialFollowerState
        }

newtype NodeFollowerState = NodeFollowerState { _unNodeFollowerState :: Map FollowerID Int }
    deriving (Show)

_NodeFollowerState :: Iso' NodeFollowerState (Map FollowerID Int)
_NodeFollowerState = iso _unNodeFollowerState NodeFollowerState

initialFollowerState :: NodeFollowerState
initialFollowerState = NodeFollowerState Map.empty

newtype FollowerID = FollowerID Int
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Integral, Enum, Real, Num, Pretty)

makeLenses 'AppState

data MockServerLogMsg =
    StartingSlotCoordination
    | NoRandomTxGeneration
    | StartingRandomTx
    | KeepingOldBlocks
    | RemovingOldBlocks
    | StartingMockServer Int
    | ProcessingChainEvent ChainEvent
    deriving (Generic, Show, ToJSON, FromJSON)

instance Pretty MockServerLogMsg where
    pretty = \case
        NoRandomTxGeneration     -> "Not creating random transactions"
        StartingRandomTx         -> "Starting random transaction generation thread"
        KeepingOldBlocks         -> "Not starting block reaper thread (old blocks will be retained in-memory forever"
        RemovingOldBlocks        -> "Starting block reaper thread (old blocks will be removed)"
        StartingMockServer p     -> "Starting Mock Node Server on port " <+> pretty p
        StartingSlotCoordination -> "Starting slot coordination thread"
        ProcessingChainEvent e   -> "Processing chain event " <+> pretty e

instance ToObject MockServerLogMsg where
    toObject _ = \case
        NoRandomTxGeneration     ->  mkObjectStr "Not creating random transactions" ()
        StartingRandomTx         ->  mkObjectStr "Starting random transaction generation thread" ()
        KeepingOldBlocks         ->  mkObjectStr "Not starting block reaper thread (old blocks will be retained in-memory forever" ()
        RemovingOldBlocks        ->  mkObjectStr "Starting block reaper thread (old blocks will be removed)" ()
        StartingMockServer p     ->  mkObjectStr "Starting Mock Node Server on port " (Tagged @"port" p)
        StartingSlotCoordination ->  mkObjectStr "" ()
        ProcessingChainEvent e   ->  mkObjectStr "Processing chain event" (Tagged @"event" e)

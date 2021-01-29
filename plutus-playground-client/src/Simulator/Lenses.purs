module Simulator.Lenses
  ( _view
  , _actionDrag
  , _simulations
  , _evaluationResult
  , _successfulEvaluationResult
  , _lastEvaluatedSimulation
  , _blockchainVisualisationState
  , _simulationId
  , _simulationActions
  , _simulationWallets
  , _resultRollup
  , _walletKeys
  ) where

import Chain.Types as Chain
import Cursor (Cursor)
import Data.Either (Either)
import Data.Json.JsonTuple (JsonTuple)
import Data.Lens (Lens', Traversal', _Right)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import MainFrame.Types (WebData)
import Network.RemoteData (_Success)
import Playground.Types (CompilationResult, ContractCall, ContractDemo, EvaluationResult, FunctionSchema, KnownCurrency, PlaygroundError, Simulation, SimulatorWallet)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Prelude ((<$>), (<<<))
import Simulator.Types (State, View)
import Schema.Types (FormArgument)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Rollup.Types (AnnotatedTx)

_view :: Lens' State View
_view = _Newtype <<< prop (SProxy :: SProxy "view")

_actionDrag :: Lens' State (Maybe Int)
_actionDrag = _Newtype <<< prop (SProxy :: SProxy "actionDrag")

_simulations :: Lens' State (Cursor Simulation)
_simulations = _Newtype <<< prop (SProxy :: SProxy "simulations")

_evaluationResult :: Lens' State (WebData (Either PlaygroundError EvaluationResult))
_evaluationResult = _Newtype <<< prop (SProxy :: SProxy "evaluationResult")

_successfulEvaluationResult :: Traversal' State EvaluationResult
_successfulEvaluationResult = _evaluationResult <<< _Success <<< _Right

_lastEvaluatedSimulation :: Lens' State (Maybe Simulation)
_lastEvaluatedSimulation = _Newtype <<< prop (SProxy :: SProxy "lastEvaluatedSimulation")

_blockchainVisualisationState :: Lens' State Chain.State
_blockchainVisualisationState = _Newtype <<< prop (SProxy :: SProxy "blockchainVisualisationState")

-----
_simulationId :: Lens' Simulation Int
_simulationId = _Newtype <<< prop (SProxy :: SProxy "simulationId")

_simulationActions :: Lens' Simulation (Array (ContractCall FormArgument))
_simulationActions = _Newtype <<< prop (SProxy :: SProxy "simulationActions")

_simulationWallets :: Lens' Simulation (Array SimulatorWallet)
_simulationWallets = _Newtype <<< prop (SProxy :: SProxy "simulationWallets")

-----
_resultRollup :: Lens' EvaluationResult (Array (Array AnnotatedTx))
_resultRollup = _Newtype <<< prop (SProxy :: SProxy "resultRollup")

_walletKeys :: Lens' EvaluationResult (Array (JsonTuple PubKeyHash Wallet))
_walletKeys = _Newtype <<< prop (SProxy :: SProxy "walletKeys")

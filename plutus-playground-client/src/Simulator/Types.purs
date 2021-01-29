module Simulator.Types
  ( State(..)
  , View(..)
  , WebEvaluationResult
  , ChainSlot
  , Blockchain
  , Action(..)
  , WalletEvent(..)
  , DragAndDropEventType(..)
  ) where

import Analytics (class IsEvent, defaultEvent)
import Chain.Types as Chain
import Clipboard as Clipboard
import Cursor (Cursor)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Halogen.Chartist as Chartist
import Network.RemoteData (RemoteData)
import Playground.Types (EvaluationResult, PlaygroundError, Simulation)
import Plutus.V1.Ledger.Tx (Tx)
import Prelude (class Eq, class Show, show, ($))
import Schema.Types (ActionEvent(..), SimulationAction(..))
import Servant.PureScript.Ajax (AjaxError)
import ValueEditor (ValueEvent(..))
import Web.HTML.Event.DragEvent (DragEvent)

newtype State
  = State
  { view :: View
  , actionDrag :: Maybe Int
  , simulations :: Cursor Simulation
  , evaluationResult :: WebEvaluationResult
  , lastEvaluatedSimulation :: Maybe Simulation
  , blockchainVisualisationState :: Chain.State
  }

derive instance newtypeState :: Newtype State _

data View
  = WalletsAndActions
  | Transactions

derive instance eqView :: Eq View

instance showView :: Show View where
  show WalletsAndActions = "WalletsAndActions"
  show Transactions = "Transactions"

type WebEvaluationResult
  = RemoteData AjaxError (Either PlaygroundError EvaluationResult)

type ChainSlot
  = Array Tx

type Blockchain
  = Array ChainSlot

-- SimulatorAction is also defined in Playground.Types as `ContractCall FormArgument`
-- (i.e. an "action" modelled in the simulation); maybe we can rethink these names.
-- There's also SimulationAction from Schema.Types. Not ideal.
data Action
  = ChangeView View
  | AddSimulationSlot
  | SetSimulationSlot Int
  | RemoveSimulationSlot Int
  | ModifyWallets WalletEvent
  | ChangeSimulation SimulationAction
  | EvaluateActions
  | ActionDragAndDrop Int DragAndDropEventType DragEvent
  | HandleBalancesChartMessage Chartist.Message
  | ChainAction Chain.Action

data WalletEvent
  = AddWallet
  | RemoveWallet Int
  | ModifyBalance Int ValueEvent

instance actionIsEvent :: IsEvent Action where
  toEvent (ChangeView view) = Just $ (defaultEvent "SimulatorView") { label = Just $ show view }
  toEvent (ChangeSimulation (PopulateAction _ _)) = Just $ (defaultEvent "PopulateAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddAction _))) = Just $ (defaultEvent "AddAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (AddWaitAction _))) = Just $ (defaultEvent "AddWaitAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (RemoveAction _))) = Just $ (defaultEvent "RemoveAction") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletValue _ _))) = Just $ (defaultEvent "SetPayToWalletValue") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetPayToWalletRecipient _ _))) = Just $ (defaultEvent "SetPayToWalletRecipient") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitTime _ _))) = Just $ (defaultEvent "SetWaitTime") { category = Just "Action" }
  toEvent (ChangeSimulation (ModifyActions (SetWaitUntilTime _ _))) = Just $ (defaultEvent "SetWaitUntilTime") { category = Just "Action" }
  toEvent AddSimulationSlot = Just $ (defaultEvent "AddSimulationSlot") { category = Just "Simulation" }
  toEvent (SetSimulationSlot _) = Just $ (defaultEvent "SetSimulationSlot") { category = Just "Simulation" }
  toEvent (RemoveSimulationSlot _) = Just $ (defaultEvent "RemoveSimulationSlot") { category = Just "Simulation" }
  toEvent (ModifyWallets AddWallet) = Just $ (defaultEvent "AddWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (RemoveWallet _)) = Just $ (defaultEvent "RemoveWallet") { category = Just "Wallet" }
  toEvent (ModifyWallets (ModifyBalance _ (SetBalance _ _ _))) = Just $ (defaultEvent "SetBalance") { category = Just "Wallet" }
  toEvent EvaluateActions = Just $ (defaultEvent "EvaluateActions") { category = Just "Action" }
  toEvent (ActionDragAndDrop _ eventType _) = Just $ (defaultEvent (show eventType)) { category = Just "Action" }
  toEvent (HandleBalancesChartMessage _) = Nothing
  toEvent (ChainAction (Chain.FocusTx (Just _))) = Just $ (defaultEvent "BlockchainFocus") { category = Just "Transaction" }
  toEvent (ChainAction (Chain.FocusTx Nothing)) = Nothing
  toEvent (ChainAction (Chain.ClipboardAction (Clipboard.CopyToClipboard _))) = Just $ (defaultEvent "ClipboardAction") { category = Just "CopyToClipboard" }

data DragAndDropEventType
  = DragStart
  | DragEnd
  | DragEnter
  | DragOver
  | DragLeave
  | Drop

instance showDragAndDropEventType :: Show DragAndDropEventType where
  show DragStart = "DragStart"
  show DragEnd = "DragEnd"
  show DragEnter = "DragEnter"
  show DragOver = "DragOver"
  show DragLeave = "DragLeave"
  show Drop = "Drop"

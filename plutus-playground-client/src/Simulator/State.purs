module Simulator.State
  ( initialState
  , handleAction
  ) where

import AjaxUtils (ajaxErrorRefLabel)
import Animation (class MonadAnimate, animate)
import Chain.State (handleAction) as Chain
import Chain.Types (Action(..), AnnotatedBlockchain(..), _chainFocusAppearing, _txIdOf, initialState) as Chain
import Clipboard (class MonadClipboard)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Extra (zoomStateT)
import Control.Monad.Trans.Class (lift)
import Cursor (Cursor, _current)
import Cursor as Cursor
import Data.Array (deleteAt, snoc, (..))
import Data.Array.Extra (move)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.Either (Either(..))
import Data.Lens (assign, modifying, over, to, traversed, use)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (lastOf, maximumOf)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import Data.RawJson (RawJson(..))
import Data.Traversable (traverse)
import Foreign.Generic (encodeJSON)
import Language.Haskell.Interpreter (SourceCode)
import MainFrame.Lenses (_functionSchema, _knownCurrencies)
import MainFrame.MonadApp (class MonadApp, editorGetContents, postEvaluation, preventDefault, resizeBalancesChart, scrollIntoView, setDataTransferData, setDropEffect)
import MainFrame.View (simulatorTitleRefLabel)
import Network.RemoteData (RemoteData(..), isSuccess)
import Playground.Server (SPParams_)
import Playground.Types (CompilationResult, ContractCall(..), Evaluation(..), KnownCurrency, Simulation(..), SimulatorWallet(..), _CallEndpoint, _FunctionSchema)
import Plutus.V1.Ledger.Value (Value)
import Prelude (class Applicative, Unit, add, bind, discard, identity, join, one, pure, show, unit, void, when, zero, (+), ($), (==), (<>), (<$>), (<*>), (>>=), (<<<))
import Schema.Types (Expression, FormArgument, SimulationAction(..), formArgumentToJson, handleActionEvent, handleFormEvent, handleValueEvent, mkInitialValue, traverseFunctionSchema)
import Servant.PureScript.Settings (SPSettings_)
import Simulator.Lenses (_actionDrag, _blockchainVisualisationState, _evaluationResult, _lastEvaluatedSimulation, _resultRollup, _simulationActions, _simulationId, _simulationWallets, _simulations, _successfulEvaluationResult, _view)
import Simulator.Types (Action(..), DragAndDropEventType(..), State(..), View(..), WalletEvent(..))
import Simulator.View (simulationsErrorRefLabel)
import Validation (_argumentValues, _argument)
import Wallet.Emulator.Wallet (Wallet(Wallet))
import Wallet.Lenses (_simulatorWalletBalance, _simulatorWalletWallet, _walletId)
import Web.HTML.Event.DataTransfer as DataTransfer

initialState :: State
initialState =
  State
    { view: WalletsAndActions
    , actionDrag: Nothing
    , simulations: Cursor.empty
    , evaluationResult: NotAsked
    , lastEvaluatedSimulation: Nothing
    , blockchainVisualisationState: Chain.initialState
    }

handleAction ::
  forall m.
  MonadState State m =>
  MonadClipboard m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadApp m =>
  MonadAnimate m State =>
  CompilationResult -> Action -> m Unit
handleAction _ (ChangeView view) = do
  assign _view view
  when (view == Transactions) resizeBalancesChart

-- Note: the following three cases involve some temporary fudges that should become
-- unnecessary when we remodel and have one evaluationResult per simulation. In
-- particular: we prevent simulation changes while the evaluationResult is Loading,
-- and switch to the simulations view (from transactions) following any change
handleAction compilationResult AddSimulationSlot = do
  evaluationResult <- use _evaluationResult
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      knownCurrencies <- peruse _knownCurrencies compilationResult
      modifying _simulations
        ( \simulations ->
            let
              maxsimulationId = fromMaybe 0 $ maximumOf (traversed <<< _simulationId) simulations

              simulationId = maxsimulationId + 1
            in
              Cursor.snoc simulations (mkSimulation knownCurrencies simulationId)
        )
      assign _view WalletsAndActions

handleAction _ (SetSimulationSlot index) = do
  evaluationResult <- use _evaluationResult
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      modifying _simulations (Cursor.setIndex index)
      assign _view WalletsAndActions

handleAction _ (RemoveSimulationSlot index) = do
  evaluationResult <- use _evaluationResult
  case evaluationResult of
    Loading -> pure unit
    _ -> do
      simulations <- use _simulations
      if (Cursor.getIndex simulations) == index then
        assign _view WalletsAndActions
      else
        pure unit
      modifying _simulations (Cursor.deleteAt index)

handleAction compilationResult (ModifyWallets action) = do
  knownCurrencies <- peruse _knownCurrencies compilationResult
  modifying (_simulations <<< _current <<< _simulationWallets) (handleActionWalletEvent (mkSimulatorWallet knownCurrencies) action)

handleAction compilationResult (ChangeSimulation subaction) = do
  knownCurrencies <- peruse _knownCurrencies compilationResult
  let
    initialValue = mkInitialValue knownCurrencies zero
  modifying (_simulations <<< _current <<< _simulationActions) (handleSimulationAction initialValue subaction)

handleAction _ EvaluateActions =
  void
    $ runMaybeT
    $ do
        simulation <- peruse (_simulations <<< _current)
        evaluation <-
          MaybeT do
            contents <- editorGetContents
            pure $ join $ toEvaluation <$> contents <*> simulation
        assign _evaluationResult Loading
        result <- lift $ postEvaluation evaluation
        assign _evaluationResult result
        case result of
          Success (Right _) -> do
            -- on successful evaluation, update last evaluated simulation, and reset and show transactions
            when (isSuccess result) do
              assign _lastEvaluatedSimulation simulation
              assign _blockchainVisualisationState Chain.initialState
              -- preselect the first transaction (if any)
              mAnnotatedBlockchain <- peruse (_successfulEvaluationResult <<< _resultRollup <<< to Chain.AnnotatedBlockchain)
              txId <- (gets <<< lastOf) (_successfulEvaluationResult <<< _resultRollup <<< traversed <<< traversed <<< Chain._txIdOf)
              lift $ zoomStateT _blockchainVisualisationState $ Chain.handleAction (Chain.FocusTx txId) mAnnotatedBlockchain
              assign _view Transactions
              lift $ scrollIntoView simulatorTitleRefLabel
          Success (Left _) -> do
            -- on failed evaluation, scroll the error pane into view
            lift $ scrollIntoView simulationsErrorRefLabel
          Failure _ -> do
            -- on failed response, scroll the ajax error pane into view
            lift $ scrollIntoView ajaxErrorRefLabel
          _ -> pure unit
        pure unit

handleAction _ (ActionDragAndDrop index DragStart event) = do
  setDataTransferData event textPlain (show index)
  assign _actionDrag (Just index)

handleAction _ (ActionDragAndDrop _ DragEnd event) = assign _actionDrag Nothing

handleAction _ (ActionDragAndDrop _ DragEnter event) = do
  preventDefault event
  setDropEffect DataTransfer.Move event

handleAction _ (ActionDragAndDrop _ DragOver event) = do
  preventDefault event
  setDropEffect DataTransfer.Move event

handleAction _ (ActionDragAndDrop _ DragLeave event) = pure unit

handleAction _ (ActionDragAndDrop destination Drop event) = do
  use _actionDrag
    >>= case _ of
        Just source -> modifying (_simulations <<< _current <<< _simulationActions) (move source destination)
        _ -> pure unit
  preventDefault event
  assign _actionDrag Nothing

-- We just ignore most Chartist events.
handleAction _ (HandleBalancesChartMessage _) = pure unit

handleAction _ (ChainAction subaction) = do
  mAnnotatedBlockchain <-
    peruse (_successfulEvaluationResult <<< _resultRollup <<< to Chain.AnnotatedBlockchain)
  let
    wrapper = case subaction of
      (Chain.FocusTx _) -> animate (_blockchainVisualisationState <<< Chain._chainFocusAppearing)
      _ -> identity
  wrapper
    $ zoomStateT _blockchainVisualisationState
    $ Chain.handleAction subaction mAnnotatedBlockchain

defaultSimulations :: Maybe (Array KnownCurrency) -> Cursor Simulation
defaultSimulations newCurrencies = case newCurrencies of
  Just currencies -> Cursor.singleton $ mkSimulation currencies 1
  Nothing -> Cursor.empty

handleActionWalletEvent :: (BigInteger -> SimulatorWallet) -> WalletEvent -> Array SimulatorWallet -> Array SimulatorWallet
handleActionWalletEvent mkWallet AddWallet wallets =
  let
    maxWalletId = fromMaybe zero $ maximumOf (traversed <<< _simulatorWalletWallet <<< _walletId) wallets

    newWallet = mkWallet (add one maxWalletId)
  in
    snoc wallets newWallet

handleActionWalletEvent _ (RemoveWallet index) wallets = fromMaybe wallets $ deleteAt index wallets

handleActionWalletEvent _ (ModifyBalance walletIndex action) wallets =
  over
    (ix walletIndex <<< _simulatorWalletBalance)
    (handleValueEvent action)
    wallets

handleSimulationAction ::
  Value ->
  SimulationAction ->
  Array (ContractCall FormArgument) ->
  Array (ContractCall FormArgument)
handleSimulationAction _ (ModifyActions actionEvent) = handleActionEvent actionEvent

handleSimulationAction initialValue (PopulateAction n event) = do
  over
    ( ix n
        <<< _CallEndpoint
        <<< _argumentValues
        <<< _FunctionSchema
        <<< _argument
    )
    $ handleFormEvent initialValue event

mkSimulatorWallet :: Array KnownCurrency -> BigInteger -> SimulatorWallet
mkSimulatorWallet currencies walletId =
  SimulatorWallet
    { simulatorWalletWallet: Wallet { getWallet: walletId }
    , simulatorWalletBalance: mkInitialValue currencies (BigInteger.fromInt 10)
    }

mkSimulation :: Array KnownCurrency -> Int -> Simulation
mkSimulation simulationCurrencies simulationId =
  Simulation
    { simulationName: "Simulation " <> show simulationId
    , simulationId
    , simulationActions: []
    , simulationWallets: mkSimulatorWallet simulationCurrencies <<< BigInteger.fromInt <$> 1 .. 2
    }

toEvaluation :: SourceCode -> Simulation -> Maybe Evaluation
toEvaluation sourceCode (Simulation { simulationActions, simulationWallets }) = do
  program <- RawJson <<< encodeJSON <$> traverse toExpression simulationActions
  pure
    $ Evaluation
        { wallets: simulationWallets
        , program
        , sourceCode
        }

toExpression :: ContractCall FormArgument -> Maybe Expression
toExpression = traverseContractCall encodeForm
  where
  encodeForm :: FormArgument -> Maybe RawJson
  encodeForm argument = (RawJson <<< encodeJSON) <$> formArgumentToJson argument

traverseContractCall ::
  forall m b a.
  Applicative m =>
  (a -> m b) ->
  ContractCall a -> m (ContractCall b)
traverseContractCall _ (AddBlocks addBlocks) = pure $ AddBlocks addBlocks

traverseContractCall _ (AddBlocksUntil addBlocksUntil) = pure $ AddBlocksUntil addBlocksUntil

traverseContractCall _ (PayToWallet payToWallet) = pure $ PayToWallet payToWallet

traverseContractCall f (CallEndpoint { caller, argumentValues: oldArgumentValues }) = rewrap <$> traverseFunctionSchema f oldArgumentValues
  where
  rewrap newArgumentValues = CallEndpoint { caller, argumentValues: newArgumentValues }

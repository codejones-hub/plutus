module MainFrame.State
  ( mkMainFrame
  , mkInitialState
  , handleAction
  ) where

import AjaxUtils (AjaxErrorPaneAction(..), ajaxErrorRefLabel, renderForeignErrors)
import Analytics (analyticsTracking)
import Animation (class MonadAnimate, animate)
import Chain.State (handleAction) as Chain
import Chain.Types (Action(..), AnnotatedBlockchain(..), _chainFocusAppearing, _txIdOf)
import Chain.Types (initialState) as Chain
import Clipboard (class MonadClipboard)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Extra (mapError)
import Control.Monad.Except.Extra (noteT)
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, withExceptT, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State.Class (class MonadState, gets)
import Control.Monad.State.Extra (zoomStateT)
import Control.Monad.Trans.Class (lift)
import Cursor (Cursor, _current)
import Cursor as Cursor
import Data.Array (catMaybes, (..))
import Data.Array (deleteAt, snoc) as Array
import Data.Array.Extra (move) as Array
import Data.Bifunctor (lmap)
import Data.BigInteger (BigInteger)
import Data.BigInteger as BigInteger
import Data.Either (Either(..), note)
import Data.Lens (Traversal', _Right, assign, modifying, over, to, traversed, use, view)
import Data.Lens.Extra (peruse)
import Data.Lens.Fold (maximumOf, lastOf, preview)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (textPlain)
import Data.Newtype (unwrap)
import Data.RawJson (RawJson(..))
import Data.String as String
import Data.Traversable (traverse)
import Editor.State (initialState) as Editor
import Editor.Lenses (_currentCodeIsCompiled, _feedbackPaneMinimised, _lastCompiledCode)
import Editor.Types (Action(..), State) as Editor
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Foreign.Generic (decodeJSON, encodeJSON)
import Gist (_GistId, gistId)
import Gists.Types (GistAction(..))
import Gists.Types as Gists
import Halogen (Component, hoist)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.Query (HalogenM)
import Language.Haskell.Interpreter (CompilationError(..), InterpreterError(..), InterpreterResult, SourceCode(..), _InterpreterResult)
import MainFrame.Lenses (_authStatus, _compilationResult, _contractDemos, _createGistResult, _currentDemoName, _currentView, _demoFilesMenuVisible, _editorState, _functionSchema, _gistErrorPaneVisible, _gistUrl, _lastSuccessfulCompilationResult, _knownCurrencies, _result, _successfulCompilationResult)
import MainFrame.MonadApp (class MonadApp, editorGetContents, editorHandleAction, editorSetAnnotations, editorSetContents, getGistByGistId, getOauthStatus, patchGistByGistId, postContract, postEvaluation, postGist, preventDefault, resizeBalancesChart, resizeEditor, runHalogenApp, saveBuffer, scrollIntoView, setDataTransferData, setDropEffect)
import MainFrame.Types (ChildSlots, HAction(..), Query, State(..), View(..), WebData)
import MainFrame.View (render)
import Monaco (IMarkerData, markerSeverity)
import Network.RemoteData (RemoteData(..), _Success, isSuccess)
import Playground.Gists (mkNewGist, playgroundGistFile, simulationGistFile)
import Playground.Server (SPParams_(..))
import Playground.Types (ContractCall(..), ContractDemo(..), Evaluation(..), KnownCurrency, Simulation(..), SimulatorWallet(..), _CallEndpoint, _FunctionSchema)
import Plutus.V1.Ledger.Value (Value)
import Prelude (class Applicative, Unit, Void, add, const, bind, discard, flip, identity, join, not, mempty, one, pure, show, unit, unless, void, when, zero, (+), ($), (&&), (==), (<>), (<$>), (<*>), (>>=), (<<<))
import Schema.Types (Expression, FormArgument, SimulationAction(..), formArgumentToJson, handleActionEvent, handleFormEvent, handleValueEvent, mkInitialValue, traverseFunctionSchema)
import Servant.PureScript.Ajax (errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Simulator.Lenses (_simulations)
import Simulator.State (initialSimulatorState)
import Simulator.View (simulationsErrorRefLabel)
import StaticData (mkContractDemos, lookupContractDemo)
import Validation (_argumentValues, _argument)
import Wallet.Emulator.Wallet (Wallet(Wallet))
import Wallet.Lenses (_simulatorWalletBalance, _simulatorWalletWallet, _walletId)
import Web.HTML.Event.DataTransfer as DataTransfer

mkMainFrame ::
  forall m n.
  MonadThrow Error n =>
  MonadEffect n =>
  MonadAff m =>
  n (Component HTML Query HAction Void m)
mkMainFrame = do
  editorState <- Editor.initialState
  initialState <- mkInitialState editorState
  pure $ hoist (flip runReaderT ajaxSettings)
    $ H.mkComponent
        { initialState: const initialState
        , render
        , eval:
            H.mkEval
              { handleAction: handleActionWithAnalyticsTracking
              , handleQuery: const $ pure Nothing
              , initialize: Just Init
              , receive: const Nothing
              , finalize: Nothing
              }
        }

ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/api/" }

mkInitialState :: forall m. MonadThrow Error m => Editor.State -> m State
mkInitialState editorState = do
  contractDemos <- mapError (\e -> error $ "Could not load demo scripts. Parsing errors: " <> show e) mkContractDemos
  pure
    $ State
        { demoFilesMenuVisible: false
        , gistErrorPaneVisible: true
        , contractDemos
        , currentDemoName: Nothing
        , authStatus: NotAsked
        , createGistResult: NotAsked
        , gistUrl: Nothing
        , currentView: Editor
        , editorState
        , compilationResult: NotAsked
        , lastSuccessfulCompilationResult: Nothing
        , simulatorState: Simulator.initialState
        }

-- TODO: use web-common withAnalytics function
handleActionWithAnalyticsTracking ::
  forall m.
  MonadEffect m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadAff m =>
  HAction -> HalogenM State HAction ChildSlots Void m Unit
handleActionWithAnalyticsTracking action = do
  liftEffect $ analyticsTracking action
  runHalogenApp $ handleAction action

handleAction ::
  forall m.
  MonadState State m =>
  MonadClipboard m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  MonadApp m =>
  MonadAnimate m State =>
  HAction -> m Unit
handleAction Init = do
  handleAction CheckAuthStatus
  editorHandleAction $ Editor.Init

handleAction Mounted = pure unit

handleAction ToggleDemoFilesMenu = modifying _demoFilesMenuVisible not

handleAction (LoadScript key) = do
  contractDemos <- use _contractDemos
  case lookupContractDemo key contractDemos of
    Nothing -> pure unit
    Just (ContractDemo { contractDemoName, contractDemoEditorContents, contractDemoSimulations, contractDemoContext }) -> do
      editorSetContents contractDemoEditorContents (Just 1)
      saveBuffer (unwrap contractDemoEditorContents)
      assign _demoFilesMenuVisible false
      assign _currentView Editor
      assign _currentDemoName (Just contractDemoName)
      assign _simulations $ Cursor.fromArray contractDemoSimulations
      assign (_editorState <<< _lastCompiledCode) (Just contractDemoEditorContents)
      assign (_editorState <<< _currentCodeIsCompiled) true
      assign _compilationResult (Success <<< Right $ contractDemoContext)
      assign _lastSuccessfulCompilationResult (Just contractDemoContext)
      assign _evaluationResult NotAsked
      assign _createGistResult NotAsked

handleAction CheckAuthStatus = do
  assign _authStatus Loading
  authResult <- getOauthStatus
  assign _authStatus authResult

handleAction (GistAction subEvent) = handleGistAction subEvent

handleAction (ChangeView view) = do
  assign _currentView view
  when (view == Editor) resizeEditor

handleAction (EditorAction action) = editorHandleAction action

handleAction CompileProgram = do
  mContents <- editorGetContents
  case mContents of
    Nothing -> pure unit
    Just contents -> do
      assign (_editorState <<< _feedbackPaneMinimised) true
      assign _compilationResult Loading
      lastSuccessfulCompilationResult <- use _lastSuccessfulCompilationResult
      newCompilationResult <- postContract contents
      assign _compilationResult newCompilationResult
      case newCompilationResult of
        Success (Left errors) -> do
          -- If there are compilation errors, add editor annotations and expand the feedback pane.
          editorSetAnnotations $ toAnnotations errors
          assign (_editorState <<< _feedbackPaneMinimised) false
        Success (Right compilationResult) ->
          -- If compilation was successful, clear editor annotations and save the successful result.
          when (isSuccess newCompilationResult) do
            editorSetAnnotations []
            assign (_editorState <<< _currentCodeIsCompiled) true
            assign (_editorState <<< _lastCompiledCode) (Just contents)
            assign _lastSuccessfulCompilationResult (Just compilationResult)
            -- If we have a result with new signatures, we can only hold onto any old actions if
            -- the signatures still match. Any change means we'll have to clear out the existing
            -- simulation. Same thing for currencies. Potentially we could be smarter about this.
            -- But for now, let's at least be correct.
            -- Note we test against the last _successful_ compilation result, so that a failed
            -- compilation in between times doesn't unnecessarily wipe the old actions.
            let
              newSignatures = preview (_details <<< _functionSchema) newCompilationResult

              newCurrencies = preview (_details <<< _knownCurrencies) newCompilationResult
            case lastSuccessfulCompilationResult of
              Nothing -> assign _simulations $ defaultSimulations newCurrencies
              Just oldCompilationResult -> do
                let
                  oldSignatures = preview (_result <<< _functionSchema) (unwrap oldCompilationResult)

                  oldCurrencies = preview (_result <<< _knownCurrencies) (unwrap oldCompilationResult)
                unless
                  (oldSignatures == newSignatures && oldCurrencies == newCurrencies)
                  (assign _simulations $ defaultSimulations newCurrencies)
        _ -> pure unit
      pure unit

handleAction (SimulatorAction action) = do
  compilationResult <- use _successfulCompilationResult
  Simulator.handleAction compilationResult action

_details :: forall a. Traversal' (WebData (Either InterpreterError (InterpreterResult a))) a
_details = _Success <<< _Right <<< _InterpreterResult <<< _result

handleGistAction :: forall m. MonadApp m => MonadState State m => GistAction -> m Unit
handleGistAction PublishGist = do
  void
    $ runMaybeT do
        mContents <- lift $ editorGetContents
        simulations <- use _simulations
        newGist <- hoistMaybe $ mkNewGist { source: mContents, simulations }
        mGist <- use _createGistResult
        assign _createGistResult Loading
        newResult <-
          lift
            $ case preview (_Success <<< gistId) mGist of
                Nothing -> postGist newGist
                Just existingGistId -> patchGistByGistId newGist existingGistId
        assign _createGistResult newResult
        gistId <- hoistMaybe $ preview (_Success <<< gistId <<< _GistId) newResult
        assign _gistUrl (Just gistId)
        when (isSuccess newResult) do
          assign _currentView Editor
          assign _currentDemoName Nothing

handleGistAction (SetGistUrl newGistUrl) = assign _gistUrl (Just newGistUrl)

handleGistAction LoadGist =
  void $ runExceptT
    $ do
        mGistId <- ExceptT (note "Gist Url not set." <$> use _gistUrl)
        eGistId <- except $ Gists.parseGistUrl mGistId
        --
        assign _createGistResult Loading
        assign _gistErrorPaneVisible true
        aGist <- lift $ getGistByGistId eGistId
        assign _createGistResult aGist
        when (isSuccess aGist) do
          assign _currentView Editor
          assign _currentDemoName Nothing
        gist <- ExceptT $ pure $ toEither (Left "Gist not loaded.") $ lmap errorToString aGist
        --
        -- Load the source, if available.
        content <- noteT "Source not found in gist." $ view playgroundGistFile gist
        lift $ editorSetContents (SourceCode content) (Just 1)
        lift $ saveBuffer content
        assign _simulations Cursor.empty
        assign _evaluationResult NotAsked
        --
        -- Load the simulation, if available.
        simulationString <- noteT "Simulation not found in gist." $ view simulationGistFile gist
        simulations <- mapExceptT (pure <<< unwrap) $ withExceptT renderForeignErrors $ decodeJSON simulationString
        assign _simulations simulations
  where
  toEither :: forall e a. Either e a -> RemoteData e a -> Either e a
  toEither _ (Success a) = Right a

  toEither _ (Failure e) = Left e

  toEither x Loading = x

  toEither x NotAsked = x

handleGistAction (AjaxErrorPaneAction CloseErrorPane) = assign _gistErrorPaneVisible false

toAnnotations :: InterpreterError -> Array IMarkerData
toAnnotations (TimeoutError _) = []

toAnnotations (CompilationErrors errors) = catMaybes (toAnnotation <$> errors)

toAnnotation :: CompilationError -> Maybe IMarkerData
toAnnotation (RawError _) = Nothing

toAnnotation (CompilationError { row, column, text }) =
  Just
    { severity: markerSeverity "Error"
    , message: String.joinWith "\\n" text
    , startLineNumber: row
    , startColumn: column
    , endLineNumber: row
    , endColumn: column
    , code: mempty
    , source: mempty
    }

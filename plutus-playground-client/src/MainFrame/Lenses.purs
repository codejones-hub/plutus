module MainFrame.Lenses
  ( _demoFilesMenuVisible
  , _gistErrorPaneVisible
  , _contractDemos
  , _currentDemoName
  , _authStatus
  , _createGistResult
  , _gistUrl
  , _currentView
  , _editorState
  , _compilationResult
  , _successfulCompilationResult
  , _lastSuccessfulCompilationResult
  , _simulatorState
  , _editorSlot
  , _balancesChartSlot
  , _contractDemoEditorContents
  , _functionSchema
  , _knownCurrencies
  , _result
  , _warnings
  , getKnownCurrencies
  ) where

import Auth (AuthStatus)
import Control.Monad.State.Class (class MonadState)
import Data.Either (Either)
import Data.Lens (Lens', Traversal', _Right)
import Data.Lens.Extra (peruse)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (SProxy(..))
import Editor.Types (State) as Editor
import Gist (Gist)
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult, SourceCode, _InterpreterResult)
import MainFrame.Types (State, View, WebData)
import Network.RemoteData (_Success)
import Playground.Types (CompilationResult, ContractDemo, FunctionSchema, KnownCurrency)
import Prelude ((<$>), (<<<))
import Schema (FormSchema)
import Simulator.Types as Simulator

_demoFilesMenuVisible :: Lens' State Boolean
_demoFilesMenuVisible = _Newtype <<< prop (SProxy :: SProxy "demoFilesMenuVisible")

_gistErrorPaneVisible :: Lens' State Boolean
_gistErrorPaneVisible = _Newtype <<< prop (SProxy :: SProxy "gistErrorPaneVisible")

_contractDemos :: Lens' State (Array ContractDemo)
_contractDemos = _Newtype <<< prop (SProxy :: SProxy "contractDemos")

_currentDemoName :: Lens' State (Maybe String)
_currentDemoName = _Newtype <<< prop (SProxy :: SProxy "currentDemoName")

_authStatus :: Lens' State (WebData AuthStatus)
_authStatus = _Newtype <<< prop (SProxy :: SProxy "authStatus")

_createGistResult :: Lens' State (WebData Gist)
_createGistResult = _Newtype <<< prop (SProxy :: SProxy "createGistResult")

_gistUrl :: Lens' State (Maybe String)
_gistUrl = _Newtype <<< prop (SProxy :: SProxy "gistUrl")

_currentView :: Lens' State View
_currentView = _Newtype <<< prop (SProxy :: SProxy "currentView")

_editorState :: Lens' State Editor.State
_editorState = _Newtype <<< prop (SProxy :: SProxy "editorState")

_compilationResult :: Lens' State (WebData (Either InterpreterError (InterpreterResult CompilationResult)))
_compilationResult = _Newtype <<< prop (SProxy :: SProxy "compilationResult")

_successfulCompilationResult :: Traversal' State CompilationResult
_successfulCompilationResult = _compilationResult <<< _Success <<< _Right <<< _InterpreterResult <<< _result

_lastSuccessfulCompilationResult :: Lens' State (Maybe (InterpreterResult CompilationResult))
_lastSuccessfulCompilationResult = _Newtype <<< prop (SProxy :: SProxy "lastSuccessfulCompilationResult")

_simulatorState :: Lens' State Simulator.State
_simulatorState = _Newtype <<< prop (SProxy :: SProxy "simulatorState")

------------------------------------------------------------
_editorSlot :: SProxy "editorSlot"
_editorSlot = SProxy

_balancesChartSlot :: SProxy "balancesChartSlot"
_balancesChartSlot = SProxy

------------------------------------------------------------
_contractDemoEditorContents :: Lens' ContractDemo SourceCode
_contractDemoEditorContents = _Newtype <<< prop (SProxy :: SProxy "contractDemoEditorContents")

_functionSchema :: Lens' CompilationResult (Array (FunctionSchema FormSchema))
_functionSchema = _Newtype <<< prop (SProxy :: SProxy "functionSchema")

_knownCurrencies :: Lens' CompilationResult (Array KnownCurrency)
_knownCurrencies = _Newtype <<< prop (SProxy :: SProxy "knownCurrencies")

--- Language.Haskell.Interpreter ---
_result :: forall s a. Lens' { result :: a | s } a
_result = prop (SProxy :: SProxy "result")

_warnings :: forall s a. Lens' { warnings :: a | s } a
_warnings = prop (SProxy :: SProxy "warnings")

getKnownCurrencies :: forall m. MonadState State m => m (Array KnownCurrency)
getKnownCurrencies = fromMaybe [] <$> peruse (_successfulCompilationResult <<< _knownCurrencies)

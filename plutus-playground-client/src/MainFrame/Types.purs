module MainFrame.Types
  ( State(..)
  , View(..)
  , WebData
  , WebCompilationResult
  , Query
  , HAction(..)
  , ChildSlots
  ) where

import Analytics (class IsEvent, defaultEvent, toEvent)
import Auth (AuthStatus)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Editor.Types as Editor
import Gist (Gist)
import Gists.Types (GistAction(..))
import Halogen as H
import Halogen.Chartist as Chartist
import Halogen.Monaco as Monaco
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult)
import Network.RemoteData (RemoteData)
import Playground.Types (CompilationResult, ContractDemo)
import Prelude (class Eq, class Show, Unit, show, ($))
import Servant.PureScript.Ajax (AjaxError)
import Simulator.Types as Simulator
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen as Gen

newtype State
  = State
  { demoFilesMenuVisible :: Boolean
  , gistErrorPaneVisible :: Boolean
  -- Demo files.
  , contractDemos :: Array ContractDemo
  , currentDemoName :: Maybe String
  -- Gist support.
  , authStatus :: WebData AuthStatus
  , createGistResult :: WebData Gist
  , gistUrl :: Maybe String
  -- Navigation.
  , currentView :: View
  -- Editor.
  , editorState :: Editor.State
  , compilationResult :: WebCompilationResult
  , lastSuccessfulCompilationResult :: Maybe (InterpreterResult CompilationResult)
  -- Simulator.
  , simulatorState :: Simulator.State
  }

derive instance newtypeState :: Newtype State _

data View
  = Editor
  | Simulator

derive instance eqView :: Eq View

derive instance genericView :: Generic View _

instance arbitraryView :: Arbitrary View where
  arbitrary = Gen.elements (Editor :| [ Simulator ])

instance showView :: Show View where
  show Editor = "Editor"
  show Simulator = "Simulator"

type WebData
  = RemoteData AjaxError

type WebCompilationResult
  = WebData (Either InterpreterError (InterpreterResult CompilationResult))

data Query a

data HAction
  = Init
  | Mounted
  -- Demo files.
  | ToggleDemoFilesMenu
  | LoadScript String
  -- Gist support.
  | CheckAuthStatus
  | GistAction GistAction
  -- Navigation.
  | ChangeView View
  -- Editor.
  | EditorAction Editor.Action
  | CompileProgram
  -- Simulator.
  | SimulatorAction Simulator.Action

type ChildSlots
  = ( editorSlot :: H.Slot Monaco.Query Monaco.Message Unit
    , balancesChartSlot :: H.Slot Chartist.Query Chartist.Message Unit
    )

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent HAction where
  toEvent Init = Nothing
  toEvent Mounted = Just $ defaultEvent "Mounted"
  -- Demo files.
  toEvent ToggleDemoFilesMenu = Nothing
  toEvent (LoadScript script) = Just $ (defaultEvent "LoadScript") { label = Just script }
  -- Gist support.
  toEvent CheckAuthStatus = Nothing
  -- TODO: put these instances in Gists.Types and replace them here with
  -- `toEvent (GistAction gistAction) = toEvent gistAction`
  toEvent (GistAction PublishGist) = Just $ (defaultEvent "Publish") { category = Just "Gist" }
  toEvent (GistAction (SetGistUrl _)) = Nothing
  toEvent (GistAction LoadGist) = Just $ (defaultEvent "LoadGist") { category = Just "Gist" }
  toEvent (GistAction (AjaxErrorPaneAction _)) = Nothing
  -- Navigation.
  toEvent (ChangeView view) = Just $ (defaultEvent "View") { label = Just $ show view }
  -- Editor.
  toEvent (EditorAction (Editor.HandleDropEvent _)) = Just $ defaultEvent "DropScript"
  toEvent (EditorAction action) = Just $ (defaultEvent "ConfigureEditor")
  toEvent CompileProgram = Just $ defaultEvent "CompileProgram"
  -- Simulator.
  toEvent (SimulatorAction simulatorAction) = toEvent simulatorAction

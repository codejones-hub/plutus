module MainFrame.View (render) where

import Bootstrap (active, btn, containerFluid, hidden, justifyContentBetween, mlAuto, mrAuto, navItem, navLink, navbar, navbarBrand, navbarExpand, navbarNav, navbarText, nbsp)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Editor.Lenses (_currentCodeIsCompiled, _keyBindings)
import Editor.Types as Editor
import Editor.View (compileButton, editorPreferencesSelect, simulateButton, editorPane, editorFeedback)
import Effect.Aff.Class (class MonadAff)
import Gists.View (gistControls)
import Halogen.HTML (ClassName(ClassName), ComponentHTML, HTML, a, button, div, footer, h1_, img, label_, main, nav, p_, span, text, ul, li)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Extra (mapComponent)
import Halogen.HTML.Properties (class_, classes, height, href, src, target, width)
import Icons (Icon(..), icon)
import Language.Haskell.Interpreter (_InterpreterResult, _SourceCode)
import MainFrame.Lenses (_contractDemoEditorContents, _currentView, _lastSuccessfulCompilationResult, _result, _simulatorState)
import MainFrame.Types (ChildSlots, HAction(..), State(..), View(..), WebCompilationResult)
import Playground.Types (ContractDemo(..))
import Prelude (class Eq, const, ($), (<$>), (<<<), (==))
import Simulator.View (simulatorTitle, simulatorWrapper)
import StaticData (bufferLocalStorageKey, lookupContractDemo)

foreign import plutusLogo :: String

render :: forall m. MonadAff m => State -> ComponentHTML HAction ChildSlots m
render state@(State { contractDemos, currentView, editorState, compilationResult }) =
  div
    [ class_ $ ClassName "frame" ]
    [ releaseBanner
    , mainHeader
    , subHeader state
    , editorMain contractDemos currentView editorState compilationResult
    , simulatorMain state
    , mainFooter
    ]

releaseBanner :: forall p. HTML p HAction
releaseBanner =
  div
    [ class_ $ ClassName "release-banner" ]
    [ text "Plutus Refresh - Updated 25th January 2021" ]

mainHeader :: forall p. HTML p HAction
mainHeader =
  nav
    [ classes [ navbar, navbarExpand, justifyContentBetween, ClassName "header" ]
    ]
    [ span [ class_ navbarBrand ]
        [ img
            [ height 36
            , width 36
            , src plutusLogo
            ]
        , text
            "Plutus playground"
        ]
    , documentationLinksPane
    ]

documentationLinksPane :: forall p i. HTML p i
documentationLinksPane =
  ul
    [ class_ navbarNav ]
    (makeNavItem <$> links)
  where
  links =
    [ text "Getting Started" /\ "https://developers.cardano.org/en/programming-languages/plutus/getting-started/"
    , text "Tutorial" /\ "./tutorial/index.html"
    , text "API" /\ "./tutorial/haddock/index.html"
    , text "Privacy" /\ "https://static.iohk.io/docs/data-protection/iohk-data-protection-gdpr-policy.pdf"
    ]

subHeader :: forall m. MonadAff m => State -> ComponentHTML HAction ChildSlots m
subHeader state@(State { demoFilesMenuVisible, contractDemos, currentDemoName }) =
  nav
    [ classes [ navbar, navbarExpand, justifyContentBetween, ClassName "sub-header" ] ]
    [ a
        [ classes buttonClasses
        , onClick $ const $ Just $ ToggleDemoFilesMenu
        ]
        [ buttonIcon ]
    , contractDemosPane demoFilesMenuVisible contractDemos currentDemoName
    , GistAction <$> gistControls (unwrap state)
    ]
  where
  buttonClasses =
    if demoFilesMenuVisible then
      [ btn, buttonClass, ClassName "open" ]
    else
      [ btn, buttonClass ]

  buttonClass = ClassName "menu-button"

  buttonIcon =
    if demoFilesMenuVisible then
      icon Close
    else
      icon Bars

contractDemosPane :: forall p. Boolean -> Array ContractDemo -> Maybe String -> HTML p HAction
contractDemosPane demoFilesMenuOpen contractDemos currentDemoName =
  div
    [ classes demoPaneClasses ]
    [ span
        [ class_ navbarText ]
        [ text "Demo files" ]
    , ul
        [ class_ navbarNav ]
        (demoScriptNavItem currentDemoName <$> contractDemos)
    ]
  where
  demoPaneClasses =
    if demoFilesMenuOpen then
      [ navbarNav, ClassName "menu", ClassName "open" ]
    else
      [ navbarNav, ClassName "menu" ]

demoScriptNavItem :: forall p. Maybe String -> ContractDemo -> HTML p HAction
demoScriptNavItem currentDemoName (ContractDemo { contractDemoName }) =
  li
    [ class_ navItem ]
    [ a
        [ classes navLinkClasses
        , onClick $ const $ Just $ LoadScript contractDemoName
        ]
        [ text contractDemoName ]
    ]
  where
  navLinkClasses = case currentDemoName of
    Just name ->
      if contractDemoName == name then
        [ active, navLink ]
      else
        [ navLink ]
    Nothing -> [ navLink ]

editorMain :: forall m. MonadAff m => Array ContractDemo -> View -> Editor.State -> WebCompilationResult -> ComponentHTML HAction ChildSlots m
editorMain contractDemos currentView editorState compilationResult =
  main
    [ classes $ mainComponentClasses currentView Editor ]
    [ div
        [ class_ $ ClassName "main-header" ]
        [ h1_ [ text "Editor" ]
        , button [ classes [ btn, ClassName "hidden" ] ] [ nbsp ]
        ] -- invisible button so the height matches the simulator header
    , editorWrapper contractDemos currentView editorState compilationResult
    ]

simulatorMain :: forall m. MonadAff m => State -> ComponentHTML HAction ChildSlots m
simulatorMain state =
  let
    currentView = view _currentView state

    lastSuccessfulCompilationResult = view _lastSuccessfulCompilationResult state

    simulatorState = view _simulatorState state
  in
    main
      [ classes $ mainComponentClasses currentView Simulator ]
      [ simulatorTitle
      , case lastSuccessfulCompilationResult of
          Just interpreterResult ->
            let
              compilationResult = view (_InterpreterResult <<< _result) interpreterResult
            in
              mapComponent SimulatorAction $ simulatorWrapper compilationResult simulatorState
          -- note: this should be impossible (rethink the model?)
          Nothing -> p_ [ text "You must compile a contract before you can run the simulator." ]
      ]

mainComponentClasses :: forall view. Eq view => view -> view -> Array (ClassName)
mainComponentClasses currentView targetView =
  if currentView == targetView then
    [ containerFluid, ClassName "main" ]
  else
    [ containerFluid, hidden, ClassName "main" ]

editorWrapper :: forall m. MonadAff m => Array ContractDemo -> View -> Editor.State -> WebCompilationResult -> ComponentHTML HAction ChildSlots m
editorWrapper contractDemos currentView editorState compilationResult =
  div
    [ classes [ ClassName "main-body", ClassName "editor" ] ]
    [ div
        [ class_ $ ClassName "editor-controls" ]
        [ div
            [ class_ $ ClassName "key-bindings" ]
            [ label_ [ text "Key Bindings" ]
            , mapComponent EditorAction $ editorPreferencesSelect (view _keyBindings editorState)
            ]
        , div
            [ class_ $ ClassName "editor-buttons" ]
            [ compileButton compilationResult
            , simulateButton (view _currentCodeIsCompiled editorState) compilationResult
            ]
        ]
    , mapComponent EditorAction $ editorPane defaultContents bufferLocalStorageKey editorState
    , mapComponent EditorAction $ editorFeedback editorState compilationResult
    ]
  where
  defaultContents :: Maybe String
  defaultContents = view (_contractDemoEditorContents <<< _SourceCode) <$> lookupContractDemo "Vesting" contractDemos

mainFooter :: forall p i. HTML p i
mainFooter =
  footer
    [ classes [ navbar, navbarExpand, ClassName "footer" ]
    ]
    [ div
        [ classes [ navbarNav, mrAuto ] ]
        [ makeNavItem $ text "cardano.org" /\ "https://cardano.org/"
        , makeNavItem $ text "iohk.io" /\ "https://iohk.io/"
        ]
    , div
        [ classes [ navbarNav ] ]
        [ copyright
        , nbsp
        , text "2020 IOHK Ltd."
        ]
    , div
        [ classes [ navbarNav, mlAuto ] ]
        [ makeNavItem $ text "GitHub" /\ "https://github.com/input-output-hk/plutus"
        , makeNavItem $ text "Twitter" /\ "https://twitter.com/hashtag/Plutus"
        , makeNavItem $ text "Feedback" /\ "https://input-output.typeform.com/to/gQ0t9ep5"
        ]
    ]

makeNavItem :: forall p i. HTML p i /\ String -> HTML p i
makeNavItem (label /\ link) =
  span
    [ classes [ navItem ] ]
    [ a
        [ class_ navLink
        , href link
        , target "_blank"
        ]
        [ label ]
    ]

copyright :: forall p i. HTML p i
copyright = text "\x00A9"

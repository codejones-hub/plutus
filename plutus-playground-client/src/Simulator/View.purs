module Simulator.View
  ( simulatorWrapper
  , simulationsErrorRefLabel
  ) where

import Action.View (actionsPane)
import Action.Validation (actionIsValid)
import AjaxUtils (ajaxErrorPane)
import Bootstrap (active, alertDanger_, btn, empty, floatRight, hidden, nav, navItem, navLink)
import Cursor (Cursor, current)
import Cursor as Cursor
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Semiring (zero)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML, RefLabel(RefLabel))
import Halogen.HTML (ClassName(ClassName), HTML, IProp, a, button, code_, div, div_, p_, pre_, span, text, ul, li)
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (class_, classes, disabled, id_, ref)
import Icons (Icon(..), icon)
import Language.Haskell.Interpreter (CompilationError(..))
import Language.Haskell.Interpreter as PI
import MainFrame.Types (ChildSlots)
import Network.RemoteData (RemoteData(..))
import Playground.Types (CompilationResult(..), ContractCall, PlaygroundError(..), Simulation(..), SimulatorWallet)
import Prelude (const, map, not, pure, show, (#), ($), (/=), (<$>), (<<<), (<>), (==), (>))
import Schema.Types (FormArgument, mkInitialValue)
import Simulator.Types (Action(..), State(..), View(..), WebEvaluationResult)
import Transaction.View (evaluationPane)
import Wallet.View (walletsPane)
import Web.Event.Event (Event)

simulatorWrapper :: forall m. MonadAff m => CompilationResult -> State -> ComponentHTML Action ChildSlots m
simulatorWrapper compilationResult state@(State { simulations }) =
  div
    [ classes [ ClassName "main-body", ClassName "simulator" ] ] case current simulations of
    Just (Simulation simulation) ->
      [ div
          [ class_ $ ClassName "simulations" ]
          [ simulationsNav simulations
          , simulationPane compilationResult (wrap simulation) state
          ]
      ]
    -- if there aren't any simulations, we shouldn't be here; consider how to remodel
    Nothing -> []

simulationsNav :: forall p. Cursor Simulation -> HTML p Action
simulationsNav simulations =
  ul
    [ classes [ nav, ClassName "nav-tabs" ]
    ]
    ( ( simulations
          # Cursor.mapWithIndex (simulationNavItem (Cursor.length simulations > 1) (Cursor.getIndex simulations))
          # Cursor.toArray
          # Array.concat
      )
        <> [ addSimulationControl ]
    )

simulationNavItem :: forall p. Boolean -> Int -> Int -> Simulation -> Array (HTML p Action)
simulationNavItem canClose activeIndex index (Simulation { simulationName }) =
  [ li
      [ id_ $ "simulation-nav-item-" <> show index
      , class_ navItem
      ]
      [ a
          [ classes navLinkClasses
          , onClick $ const $ Just $ SetSimulationSlot index
          ]
          [ text simulationName ]
      , if canClose then
          button
            [ classes [ btn, navItemButtonClass ]
            , onClick $ const $ Just $ RemoveSimulationSlot index
            ]
            [ icon Close ]
        else
          empty
      ]
  ]
  where
  navLinkClasses = if activeIndex == index then [ navLink, active ] else [ navLink ]

addSimulationControl :: forall p. HTML p Action
addSimulationControl =
  li
    [ id_ "simulation-nav-item-add"
    , class_ navItem
    ]
    [ span
        [ class_ navLink ]
        [ button
            [ classes [ btn, navItemButtonClass ]
            , onClick $ const $ Just $ AddSimulationSlot
            ]
            [ icon Plus ]
        ]
    ]

simulationPane :: forall m. MonadAff m => CompilationResult -> Simulation -> State -> ComponentHTML Action ChildSlots m
simulationPane (CompilationResult { knownCurrencies, functionSchema }) (Simulation { simulationWallets, simulationActions }) (State { view, actionDrag, simulations, lastEvaluatedSimulation, evaluationResult, blockchainVisualisationState }) =
  let
    initialValue = mkInitialValue knownCurrencies zero
  in
    div
      [ class_ $ ClassName "simulation" ]
      [ div
          (if view == WalletsAndActions then [] else [ class_ hidden ])
          [ div
              [ classes [ ClassName "simulation-controls", floatRight ] ]
              [ evaluateActionsButton simulationWallets simulationActions evaluationResult
              , viewTransactionsButton simulations lastEvaluatedSimulation evaluationResult
              ]
          , walletsPane functionSchema initialValue simulationWallets
          , actionsPane actionDrag simulationWallets simulationActions
          , div
              [ classes [ ClassName "simulation-controls" ] ]
              [ evaluateActionsButton simulationWallets simulationActions evaluationResult
              , viewTransactionsButton simulations lastEvaluatedSimulation evaluationResult
              ]
          , case evaluationResult of
              Failure error -> ajaxErrorPane error
              Success (Left error) -> actionsErrorPane error
              _ -> empty
          ]
      , div
          (if view == Transactions then [] else [ class_ hidden ]) case evaluationResult of
          Success (Right evaluation) -> [ evaluationPane blockchainVisualisationState evaluation ]
          -- all other states should be impossible when simulationView == Transactions;
          -- consider how to model this better
          _ -> [ p_ [ text "You must evaluate your simulation before you can view the evaluation results." ] ]
      ]

evaluateActionsButton :: forall p. Array SimulatorWallet -> Array (ContractCall FormArgument) -> WebEvaluationResult -> HTML p Action
evaluateActionsButton simulationWallets simulationActions evaluationResult =
  button
    [ classes [ btn, ClassName "btn-green" ]
    , disabled $ not valid
    , onClick $ const $ Just EvaluateActions
    ]
    [ btnText evaluationResult valid ]
  where
  valid = (Array.all <<< actionIsValid) simulationWallets simulationActions

  btnText Loading _ = icon Spinner

  btnText _ false = text "Fix Errors"

  btnText _ _ = text "Evaluate"

viewTransactionsButton :: forall p. Cursor Simulation -> Maybe Simulation -> WebEvaluationResult -> HTML p Action
viewTransactionsButton simulations lastEvaluatedSimulation evaluationResult =
  button
    [ classes [ btn, ClassName "btn-turquoise" ]
    , disabled isDisabled
    , onClick $ const $ Just $ ChangeView Transactions
    ]
    [ text "Transactions" ]
  where
  isDisabled = case evaluationResult of
    Success _ -> (current simulations) /= lastEvaluatedSimulation
    _ -> true

actionsErrorPane :: forall p i. PlaygroundError -> HTML p i
actionsErrorPane error =
  div
    [ class_ $ ClassName "ajax-error"
    , ref simulationsErrorRefLabel
    ]
    [ alertDanger_
        ( (div_ <<< pure)
            <$> (showPlaygroundError error <> [ text "Please try again or contact support for assistance." ])
        )
    ]

------------------------------------------------------------
-- | There's a few errors that make sense to display nicely, others should not occur so lets
-- | not deal with them.
showPlaygroundError :: forall p i. PlaygroundError -> Array (HTML p i)
showPlaygroundError (CompilationErrors errors) =
  [ text "Compilation Errors" ]
    <> (showCompilationError <$> errors)

showPlaygroundError (InterpreterError (PI.TimeoutError error)) =
  [ text "Interpreter Timed Out"
  , code_ [ text error ]
  ]

showPlaygroundError (InterpreterError (PI.CompilationErrors errors)) =
  [ text "Interpreter Errors" ]
    <> (showCompilationError <$> errors)

showPlaygroundError (RollupError error) =
  [ text "Error Calculating Final Blockchain State"
  , code_ [ text error ]
  ]

showPlaygroundError (OtherError error) =
  [ text "Unknown Evaluation Error"
  , code_ [ text error ]
  ]

showPlaygroundError (JsonDecodingError { expected, decodingError, input }) =
  [ text "Decoding Error"
  , code_ [ text $ "Expected: " <> expected ]
  , code_ [ text $ "Error: " <> decodingError ]
  , code_ [ text $ "Input: " <> input ]
  ]

showCompilationError :: forall p i. CompilationError -> HTML p i
showCompilationError (RawError error) = code_ [ text error ]

showCompilationError (CompilationError { text: errors }) = pre_ [ text (String.joinWith "\n" errors) ]

onIntInput :: forall i r. (Int -> i) -> IProp ( onInput :: Event, value :: String | r ) i
onIntInput f = onValueInput $ map f <<< Int.fromString

------------------------------------------------------------
simulationsErrorRefLabel :: RefLabel
simulationsErrorRefLabel = RefLabel "simulation-errors"

navItemButtonClass :: ClassName
navItemButtonClass = ClassName "simulation-nav-item-control"

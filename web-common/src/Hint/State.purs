module Hint.State (component, hint) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Foldable (for_)
import Data.Lens (assign, set, use)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (random)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component, HalogenM, Slot, get, getHTMLElementRef, liftEffect, mkComponent, modify_)
import Halogen as H
import Halogen.HTML (ComponentHTML, HTML, PlainHTML, slot)
import Halogen.Query.EventSource (eventListenerEventSource)
import Hint.Lenses (_active, _mPopperInstance, _content, _placement)
import Hint.Types (Action(..), Input, State, arrowRef, hintRef, popoutRef)
import Hint.View (render)
import Popper (OffsetOption(..), PaddingOption(..), Placement, PositioningStrategy(..), arrow, createPopper, defaultModifiers, defaultPreventOverflow, destroyPopper, forceUpdate, offset, pAll, preventOverflow)
import Tooltip.Types (ReferenceId(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

_hintSlot :: SProxy "hintSlot"
_hintSlot = SProxy

hint ::
  forall slots m action.
  MonadAff m =>
  String ->
  Placement ->
  PlainHTML ->
  ComponentHTML action ( hintSlot :: forall query. Slot query Void String | slots ) m
hint ref placement content = slot _hintSlot ref component { content, placement } absurd

-- hint ref placement content = slot _hintSlot (ref <> (show $ unsafePerformEffect random)) component { content, placement } absurd
initialState :: Input -> State
initialState { content, placement } =
  { content
  , placement
  , active: false
  , mPopperInstance: Nothing
  }

component ::
  forall m query.
  MonadAff m =>
  Component HTML query Input Void m
component =
  mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Init
              , finalize = Just Finalize
              , handleAction = handleAction
              , receive = \input -> Just $ OnNewInput input
              }
    }

handleAction ::
  forall m slots.
  MonadAff m =>
  Action ->
  HalogenM State Action slots Void m Unit
handleAction Init = do
  traceM "Init"
  placement <- use _placement
  mPopperInstance <-
    runMaybeT do
      arrowElem <- MaybeT $ getHTMLElementRef arrowRef
      let
        modifiers =
          defaultModifiers
            <> [ arrow arrowElem (PaddingValue $ pAll 0.0)
              , offset (OffsetValue { skidding: 0.0, distance: 8.0 })
              , preventOverflow defaultPreventOverflow
              ]
      hintElem <- MaybeT $ getHTMLElementRef hintRef
      popoutElem <- MaybeT $ getHTMLElementRef popoutRef
      popperInstance <- liftEffect $ createPopper hintElem popoutElem { placement, modifiers, strategy: Fixed }
      -- We add event listeners to the target element to know when to show and hide the tooltip. We don't store the
      -- subscriptionId to manually remove it because we are inside a component (not a subcomponent), so any
      -- subscriptions will be terminated when the component is no longer rendered.
      -- TODO: We could later implement the performance suggestions from https://popper.js.org/docs/v2/tutorial/#performance
      -- void $ MaybeT $ map pure $ H.subscribe $ eventListenerEventSource (EventType "mouseenter") (HTMLElement.toEventTarget refElem) (const $ Just Show)
      -- void $ MaybeT $ map pure $ H.subscribe $ eventListenerEventSource (EventType "focus") (HTMLElement.toEventTarget refElem) (const $ Just Show)
      -- void $ MaybeT $ map pure $ H.subscribe $ eventListenerEventSource (EventType "mouseleave") (HTMLElement.toEventTarget refElem) (const $ Just Hide)
      -- void $ MaybeT $ map pure $ H.subscribe $ eventListenerEventSource (EventType "blur") (HTMLElement.toEventTarget refElem) (const $ Just Hide)
      pure popperInstance
  assign _mPopperInstance mPopperInstance

handleAction Finalize = do
  traceM "Finalize"
  mPopperInstance <- use _mPopperInstance
  for_ mPopperInstance $ liftEffect <<< destroyPopper

-- TODO: This is being called 5 times per second per active hint. There is probably an overhead in the timer from the PAB
-- that causes 5 re-renders per second, but also we should investigate on memoize this
handleAction (OnNewInput input) = do
  traceM "hint new input"
  { active, content, placement } <- get
  modify_
    ( set _content input.content
        <<< set _placement input.placement
    )
  when active forceUpdatePopper

handleAction Open = do
  assign _active true
  forceUpdatePopper

handleAction Close = assign _active false

handleAction Toggle = do
  active <- use _active
  if active then
    handleAction Close
  else
    handleAction Open

forceUpdatePopper :: forall m slots. MonadAff m => HalogenM State Action slots Void m Unit
forceUpdatePopper = do
  mPopperInstance <- use _mPopperInstance
  for_ mPopperInstance $ liftEffect <<< forceUpdate

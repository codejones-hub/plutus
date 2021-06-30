module Hint.View (render) where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Halogen.Css (classNames, hideWhen)
import Halogen.HTML (HTML, div, div_, fromPlainHTML, span, span_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (ref)
import Halogen.HTML.Properties.ARIA (role)
import Hint.Types (Action(..), State, arrowRef, hintRef, popoutRef)
import Material.Icons as Icon

render :: forall p. State -> HTML p Action
render state =
  div [ classNames [ "inline" ] ]
    [ div
        [ classNames
            [ "cursor-pointer"
            , "inline"
            ]
        , onClick $ const $ Just Toggle
        , ref hintRef
        ]
        [ Icon.icon Icon.HelpOutline [ "text-purple", "text-xs" ] ]
    , div
        [ ref popoutRef
        , role "tooltip"
        , classNames
            ( [ "bg-white", "p-2", "rounded-sm", "shadow-flat", "min-w-36", "z-50" ]
                <> hideWhen (not state.active)
            )
        ]
        [ div
            [ ref arrowRef
            , classNames [ "popover-arrow" ]
            ]
            []
        , div
            [ classNames [ "cursor-pointer" ]
            , onClick $ const $ Just Close
            ]
            [ Icon.icon_ Icon.Close ]
        , fromPlainHTML state.content
        ]
    ]

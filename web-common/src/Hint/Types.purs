module Hint.Types where

import Data.Maybe (Maybe)
import Halogen (RefLabel(..))
import Halogen.HTML (PlainHTML)
import Popper (Placement, PopperInstance)

type State
  = { content :: PlainHTML
    , active :: Boolean
    , placement :: Placement
    , mPopperInstance :: Maybe PopperInstance
    }

type Input
  = { content :: PlainHTML
    , placement :: Placement
    }

data Action
  = Init
  | Finalize
  | OnNewInput Input
  | Open
  | Close
  | Toggle

hintRef :: RefLabel
hintRef = (RefLabel "hint")

popoutRef :: RefLabel
popoutRef = (RefLabel "popout")

arrowRef :: RefLabel
arrowRef = (RefLabel "arrow")

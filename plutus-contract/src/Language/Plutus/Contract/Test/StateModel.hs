-- This is a simple state modelling library for use with Haskell
-- QuickCheck. For documentation, see the associated slides.

{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Plutus.Contract.Test.StateModel
  ( StateModel(..)
  , Step(..)
  , Script(..)
  , runScript
  , notStuck) where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

class (Show (Action state), Monad (ActionMonad state)) =>
        StateModel state where
  data Action state
  data Ret state
  type ActionMonad state :: * -> *
  actionName      :: Action state -> String
  actionName = head . words . show
  arbitraryAction :: state -> Gen (Action state)
  shrinkAction    :: state -> Action state -> [Action state]
  shrinkAction _ _ = []
  initialState    :: state
  nextState       :: state -> Action state -> Step -> state
  nextState s _ _ = s
  precondition    :: state -> Action state -> Bool
  precondition _ _ = True
  needs           :: Action state -> [Step]
  needs _ = []
  perform         :: Action state -> [Ret state] -> ActionMonad state (Ret state)
  perform _ _ = return undefined
  postcondition   :: state -> Action state -> (Step -> Ret state) -> Ret state -> Bool
  postcondition _ _ _ _ = True
  monitoring      :: (state,state) -> Action state -> (Step -> Ret state) -> Ret state -> Property -> Property
  monitoring _ _ _ _ = id
  isFinal :: state -> Bool

newtype Step = Step Int
  deriving (Eq, Ord, Show)

newtype Script state = Script [(Step, Action state)]

instance Show (Action state) => Show (Script state) where
  showsPrec d (Script as)
    | d>10      = ("("++).showsPrec 0 (Script as).(")"++)
    | null as   = ("Script []"++)
    | otherwise = (("Script \n [")++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]

instance StateModel state => Arbitrary (Script state) where
  arbitrary = Script <$> arbActions initialState 1
    where
      arbActions :: state -> Int -> Gen [(Step, Action state)]
      arbActions s step = sized $ \n ->
        let w = n `div` 2 + 1 in
          frequency [(1, return []),
                     (w, do mact <- arbitraryAction s `suchThatMaybe` precondition s
                            case mact of
                              Just act ->
                                ((Step step,act):) <$> arbActions (nextState s act (Step step)) (step+1)
                              Nothing ->
                                return [])]

  shrink (Script as) =
    map (Script . prune . map fst) (shrinkList shrinker (withStates as))
    where shrinker ((step,act),s) = [((step,act'),s) | act' <- shrinkAction s act]

prune :: StateModel state => [(Step, Action state)] -> [(Step, Action state)]
prune = loop initialState []
  where loop _s _steps [] = []
        loop s steps ((step,act):as)
          | precondition s act
            = (step,act):loop (nextState s act step) (step:steps) as
          | otherwise
            = loop s steps as

withStates :: StateModel state => [(Step, Action state)] -> [((Step,Action state),state)]
withStates = loop initialState
  where
    loop _s [] = []
    loop s ((step,act):as) =
      ((step,act),s):loop (nextState s act step) as

stateAfter :: StateModel state => Script state -> state
stateAfter (Script script) = loop initialState script
  where
    loop s []                 = s
    loop s ((step, act) : as) = loop (nextState s act step) as

runScript :: (StateModel state, Show (Ret state)) =>
                Script state -> PropertyM (ActionMonad state) (state, [(Step, Ret state)])
runScript (Script script) = loop initialState [] script
  where
    loop s steps [] = return (s, reverse steps)
    loop s steps ((n,a):as) = do
      pre $ precondition s a
      let deps = map (getStep steps) (needs a)
      ret <- run (perform a deps)
      let name = head (words (show a))
      monitor (tabulate "Actions" [name] . classify True name)
      monitor (counterexample (show n++": "++show a++" "++show deps++" --> "++show ret))
      let s'     = nextState s a n
          steps' = (n,ret):steps
      monitor (monitoring (s,s') a (getStep steps') ret)
      assert $ postcondition s a (getStep steps) ret
      loop s' steps' as

getStep :: (Show step, Eq step) => [(step, ret)] -> step -> ret
getStep steps n = case lookup n steps of
  Just v  -> v
  Nothing -> error ("Missing step "++show n)

notStuck :: StateModel state => Script state -> Property
notStuck script
  | isFinal s = property True
  | otherwise = forAll (vectorOf 20 $ arbitraryAction s) $ any (precondition s)
  where
    s = stateAfter script


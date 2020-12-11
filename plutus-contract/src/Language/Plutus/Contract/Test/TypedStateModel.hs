-- This is a simple state modelling library for use with Haskell
-- QuickCheck. For documentation, see the associated slides.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-tabs #-}

module Language.Plutus.Contract.Test.TypedStateModel(
  StateModel(..),AnyAction(..),
  Step(..),Var(..), -- we export the constructors so that users can construct test cases
  Script(..),runScript
  , notStuck
  -- * Contract specifics
  , propRunScript
  , propRunScriptWithDistribution
) where

import qualified Control.Monad.Freer.State       as Eff
import           Control.Monad.Writer
import           Data.Aeson                      (FromJSON)
import           Data.Row                        (AllUniqueLabels, Forall)
import           Data.Row.Internal               (Unconstrained1)
import qualified Data.Text                       as Text
import           Data.Text.Prettyprint.Doc

import           Data.Typeable

import           Language.Plutus.Contract.Schema (Input, Output)
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Types  (Contract)
import qualified Wallet.Emulator                 as EM

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

class (forall a. Show (Action state a), Monad (ActionMonad state)) =>
        StateModel state where
  data Action state a
  type ActionMonad state :: * -> *
  actionName      :: Action state a -> String
  actionName = head . words . show
  arbitraryAction :: state -> Gen (AnyAction state)
  shrinkAction    :: state -> Action state a -> [AnyAction state]
  shrinkAction _ _ = []
  initialState    :: state
  nextState       :: state -> Action state a -> Var a -> state
  nextState s _ _ = s
  precondition    :: state -> Action state a -> Bool
  precondition _ _ = True
  perform         :: state -> Action state a -> LookUp -> ActionMonad state a
  perform _ _ _ = return undefined
  postcondition   :: state -> Action state a -> LookUp -> a -> Bool
  postcondition _ _ _ _ = True
  monitoring      :: (state,state) -> Action state a -> LookUp -> a -> Property -> Property
  monitoring _ _ _ _ = id
  isFinal :: state -> Bool
  isFinal _ = False

type LookUp = forall a. Typeable a => Var a -> a

type Env = [EnvEntry]

data EnvEntry where
  (:==) :: (Show a,Typeable a) => Var a -> a -> EnvEntry

infix 5 :==

deriving instance Show EnvEntry

lookUpVar :: Typeable a => Env -> Var a -> a
lookUpVar [] v = error $ "Variable "++show v++" is not bound!"
lookUpVar ((v' :== a) : env) v =
  case cast (v',a) of
    Just (v'',a') | v==v'' -> a'
    _                      -> lookUpVar env v

data AnyAction state where
  Action :: (Show a, Typeable a) => Action state a -> AnyAction state

deriving instance (forall a. Show (Action state a)) => Show (AnyAction state)

data Step state where
  (:=) :: (Show a, Typeable a) => Var a -> Action state a -> Step state

infix 5 :=

deriving instance (forall a. Show (Action state a)) => Show (Step state)

newtype Var a = Var Int
  deriving (Eq, Ord, Show)

newtype Script state = Script [Step state]

instance (forall a. Show (Action state a)) => Show (Script state) where
  showsPrec d (Script as)
    | d>10      = ("("++).showsPrec 0 (Script as).(")"++)
    | null as   = ("Script []"++)
    | otherwise = (("Script \n [")++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]


instance StateModel state => Arbitrary (Script state) where
  arbitrary = Script <$> arbActions initialState 1
    where
      arbActions :: state -> Int -> Gen [Step state]
      arbActions s step = sized $ \n ->
        let w = n `div` 2 + 1 in
          frequency [(1, return []),
                     (w, do mact <- arbitraryAction s `suchThatMaybe`
                                      \(Action act) -> precondition s act
		            case mact of
			      Just (Action act) ->
                                ((Var step := act):) <$> arbActions (nextState s act (Var step)) (step+1)
		              Nothing ->
			        return [])]

  shrink (Script as) =
    map (Script . prune . map fst) (shrinkList shrinker (withStates as))
    where shrinker ((Var i := act),s) = [((Var i := act'),s) | Action act' <- shrinkAction s act]

prune :: StateModel state => [Step state] -> [Step state]
prune = loop initialState
  where loop _s [] = []
        loop s ((var := act):as)
          | precondition s act
            = (var := act):loop (nextState s act var) as
          | otherwise
            = loop s as


withStates :: StateModel state => [Step state] -> [(Step state,state)]
withStates = loop initialState
  where
    loop _s [] = []
    loop s ((var := act):as) =
      ((var := act),s):loop (nextState s act var) as

stateAfter :: StateModel state => Script state -> state
stateAfter (Script script) = loop initialState script
  where
    loop s []                  = s
    loop s ((var := act) : as) = loop (nextState s act var) as

runScript :: StateModel state =>
                Script state -> PropertyM (ActionMonad state) (state,Env)
runScript (Script script) = loop initialState [] script
  where
    loop _s env [] = return (_s,reverse env)
    loop s env ((Var n := act):as) = do
      pre $ precondition s act
      ret <- run (perform s act (lookUpVar env))
      let name = actionName act
      monitor (tabulate "Actions" [name] . classify True ("contains "++name))
      monitor (counterexample ("Var "++show n++" := "++show act++" --> "++show ret))
      let s'   = nextState s act (Var n)
          env' = (Var n :== ret):env
      monitor (monitoring (s,s') act (lookUpVar env') ret)
      assert $ postcondition s act (lookUpVar env) ret
      loop s' env' as

notStuck :: StateModel state => Script state -> Property
notStuck script
  | isFinal s = property True
  | otherwise = forAll (vectorOf 20 $ arbitraryAction s) $ any (\(Action act) -> precondition s act)
  where
    s = stateAfter script

-- * Contract specifics

runTr ::
    ( Show e
    , AllUniqueLabels (Input s)
    , Forall (Input s) FromJSON
    , Forall (Output s) Unconstrained1
    ) =>
    InitialDistribution -> Contract s e a -> ContractTrace s e a Property -> Property
runTr distr contract tr =
    case runTraceWithDistribution distr contract tr of
        (Right (p, _), _) -> p
        (Left err, _s)    -> whenFail (print err) False

getEmulatorState :: ContractTrace s e a EM.EmulatorState
getEmulatorState = Eff.get

getContractTraceState :: ContractTrace s e a (ContractTraceState s (TraceError e) a)
getContractTraceState = Eff.get

assertPredicate ::
    forall s e a.
    ( Show e
    , Forall (Input s) Pretty
    , Forall (Output s) Pretty
    )
    => InitialDistribution
    -> TracePredicate s (TraceError e) a
    -> PropertyM (ContractTrace s e a) ()
assertPredicate dist predicate = do
    em <- run getEmulatorState
    st <- run getContractTraceState
    let r = ContractTraceResult em st
        (result, testOutputs) = runWriter $ unPredF predicate (dist, r)
    monitor (counterexample $ Text.unpack $ renderTraceContext testOutputs st)
    assert result

propRunScript ::
  ( StateModel state
  , ActionMonad state ~ ContractTrace s e a
  , Show e
  , AllUniqueLabels (Input s)
  , Forall (Input s) FromJSON
  , Forall (Input s) Pretty
  , Forall (Output s) Pretty
  , Forall (Output s) Unconstrained1
  )
  => (state -> TracePredicate s (TraceError e) a)
  -> Contract s e a
  -> PropertyM (ActionMonad state) ()
  -> Script state
  -> (state -> PropertyM (ActionMonad state) ())
  -> Property
propRunScript = propRunScriptWithDistribution defaultDist

propRunScriptWithDistribution ::
  ( StateModel state
  , ActionMonad state ~ ContractTrace s e a
  , Show e
  , AllUniqueLabels (Input s)
  , Forall (Input s) FromJSON
  , Forall (Input s) Pretty
  , Forall (Output s) Pretty
  , Forall (Output s) Unconstrained1
  )
  => InitialDistribution
  -> (state -> TracePredicate s (TraceError e) a)
  -> Contract s e a
  -> PropertyM (ActionMonad state) ()
  -> Script state
  -> (state -> PropertyM (ActionMonad state) ())
  -> Property
propRunScriptWithDistribution dist finalPredicate contract before script after =
  monadic (runTr defaultDist contract) $ do
    before
    (st, _) <- runScript script
    after st
    assertPredicate dist (finalPredicate st)


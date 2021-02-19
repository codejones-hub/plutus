-- | This module provides a framework for testing Plutus contracts built on "Test.QuickCheck". The
--   testing is model based, so to test a contract you define a type modelling the state of the
--   contract (or set of contracts) and provide an instance of the `ContractModel` class. This
--   instance specifies what operations (`Action`s) the contract supports, how they interact with
--   the model state, and how to execute them in the blockchain emulator ("Plutus.Trace.Emulator").
--   Tests are evaluated by running sequences of actions (random or user-specified) in the emulator
--   and comparing the state of the blockchain to the model state at the end.
--
--   Test cases are written in the `DL` monad, which supports mixing fixed sequences of actions with
--   random actions, making it easy to write properties like
--   /it is always possible to get all funds out of the contract/.

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Plutus.Contract.Test.ContractModel
    ( -- * Contract models
      --
      -- $contractModel
      ContractModel(..)
      -- ** Model state
    , ModelState
    , contractState
    , currentSlot
    , balances
    , forged
    , lockedFunds
    , GetModelState(..)
    , viewModelState
    , getContractState
    , viewContractState
    -- ** Spec monad
    --
    -- $specMonad
    , Spec
    , wait
    , waitUntil
    , forge
    , burn
    , deposit
    , withdraw
    , transfer
    , ($=), ($~)
    -- * Dynamic logic
    --
    -- $dynamicLogic
    , DL
    , action
    , DL.anyAction
    , DL.anyActions
    , DL.anyActions_
    , DL.stopping
    , DL.weight
    , DL.assert
    , assertModel
    , DL.monitorDL
    , forAllDL
    -- ** Random generation
    --
    -- $quantify
    , DL.forAllQ
    , module Language.Plutus.Contract.Test.DynamicLogic.Quantify
    -- * Running properties
    --
    -- $runningProperties
    , Script
    -- ** Wallet contract handles
    , HandleSpec(..)
    , HandleFun
    -- ** Running
    , propRunScript_
    , propRunScript
    , propRunScriptWithOptions
    ) where

import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Freer                                 as Eff
import           Control.Monad.Freer.Log
import           Control.Monad.Freer.State
import           Control.Monad.Writer
import qualified Data.Aeson                                          as JSON
import           Data.Foldable
import           Data.Map                                            (Map)
import qualified Data.Map                                            as Map
import           Data.Row                                            (Row)
import           Data.Typeable

import           Language.Plutus.Contract                            (Contract, HasBlockchainActions)
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Test.DynamicLogic.Monad    as DL
import           Language.Plutus.Contract.Test.DynamicLogic.Quantify (Quantifiable, Quantification, arbitraryQ, chooseQ,
                                                                      elementsQ, exactlyQ, frequencyQ, mapQ, oneofQ,
                                                                      whereQ)
import           Language.Plutus.Contract.Test.StateModel            hiding (Action, Script, arbitraryAction,
                                                                      initialState, monitoring, nextState, perform,
                                                                      precondition, shrinkAction)
import qualified Language.Plutus.Contract.Test.StateModel            as StateModel
import           Language.PlutusTx.Monoid                            (inv)
import           Ledger.Slot
import           Ledger.Value                                        (Value)
import           Plutus.Trace.Emulator                               as Trace (ContractHandle, EmulatorTrace,
                                                                               activateContractWallet)

import           Test.QuickCheck                                     hiding ((.&&.))
import qualified Test.QuickCheck                                     as QC
import           Test.QuickCheck.Monadic                             as QC

data IMap (key :: i -> j -> *) (val :: i -> j -> *) where
    IMNil  :: IMap key val
    IMCons :: (Typeable i, Typeable j) => key i j -> val i j -> IMap key val -> IMap key val

imLookup :: (Typeable i, Typeable j, Typeable key, Typeable val, Eq (key i j)) => key i j -> IMap key val -> Maybe (val i j)
imLookup _ IMNil = Nothing
imLookup k (IMCons key val m) =
    case cast (key, val) of
        Just (key', val') | key' == k -> Just val'
        _                             -> imLookup k m

data HandleSpec state where
    HandleSpec :: forall state schema err.
                  ( Typeable schema
                  , HasBlockchainActions schema
                  , ContractConstraints schema
                  , Show err
                  , Typeable err
                  , JSON.ToJSON err
                  , JSON.FromJSON err
                  )
                  => HandleKey state schema err
                  -> Wallet
                  -> Contract schema err ()
                  -> HandleSpec state

type Handles state = IMap (HandleKey state) ContractHandle

type HandleFun state = forall schema err. (Typeable schema, Typeable err) => HandleKey state schema err -> Trace.ContractHandle schema err

data ModelState state = ModelState
        { _currentSlot   :: Slot
        , _lastSlot      :: Slot
        , _balances      :: Map Wallet Value
        , _forged        :: Value
        , _contractState :: state
        }

type Script s = StateModel.Script (ModelState s)

instance Show state => Show (ModelState state) where
    show = show . _contractState   -- for now

newtype Spec state a = Spec (Eff '[State (ModelState state)] a)
    deriving (Functor, Applicative, Monad)

-- $contractModel
--
-- A contract model is a type @state@ with a `ContractModel` instance. The state type should
-- capture an abstraction of the state of the blockchain relevant to the contract (or contracts)
-- under test. During test generation and execution, the contract-specific @state@ is wrapped in the
-- `ModelState` type, which in addition to @state@ tracks common features of the blockchain, like
-- wallet balances and the current slot.

-- | A `ContractModel` instance captures everything that is needed to generate and run tests of a
--   contract or set of contracts. It specifies among other things
--
--  * what operations are supported by the contract (`Action`),
--  * when they are valid (`precondition`),
--  * how to generate random actions (`arbitraryAction`),
--  * how the operations affect the state (`nextState`), and
--  * how to run the operations in the emulator (`perform`)

class ( Typeable state
      , Show state
      , Show (Action state)
      , Eq (Action state)
      , (forall s e. Eq (HandleKey state s e))
      , (forall s e. Show (HandleKey state s e))
      ) => ContractModel state where

    -- | The type of actions that are supported by the contract. An action usually represents a single
    --   `Plutus.Trace.Emulator.callEndpoint` or a transfer of tokens, but it can be anything
    --   that can be interpreted in the `EmulatorTrace` monad.
    data Action state

    -- | To be able to call a contract endpoint from a wallet a `ContractHandle` is required.
    --   These are managed by the test framework and all the user needs to do is provide this handle
    --   key type representing the different handles that a test needs to work with, and when
    --   creating a property (see `propRunScript_`) provide a list of handle keys together with
    --   their wallets and contracts. Handle keys are indexed by the schema and error type of the
    --   contract and should be defined as a GADT. For example, a handle type for a contract with
    --   one seller and multiple buyers could look like this.
    --
    --   >  data HandleKey MyModel s e where
    --   >      Buyer  :: Wallet -> HandleKey MyModel MySchema MyError
    --   >      Seller :: HandleKey MyModel MySchema MyError
    data HandleKey state :: Row * -> * -> *

    -- | Given the current model state, provide a QuickCheck generator for a random next action.
    arbitraryAction :: ModelState state -> Gen (Action state)

    -- | The initial state, before any actions have been performed.
    initialState :: state

    -- | The `precondition` function decides if a given action is valid in a given state. Typically
    --   actions generated by `arbitraryAction` will satisfy the precondition, but if they don't
    --   they will be discarded and another action will be generated. More importantly, the
    --   preconditions are used when shrinking (see `shrinkAction`) to ensure that shrunk test cases
    --   still make sense.
    --
    --   If an explicit `action` in a `DL` script violates the precondition an error is raised.
    precondition :: ModelState state -> Action state -> Bool
    precondition _ _ = True

    -- | This is where the model logic is defined. Given an action, `nextState` specifies the
    --   effects running that action has on the model state. It runs in the `Spec` monad, which is a
    --   state monad over the `ModelState`.
    nextState :: Action state -> Spec state ()
    nextState _ = return ()

    -- | While `nextState` models the behaviour of the actions, `perform` contains the code for
    --   running the actions in the emulator (see "Plutus.Trace.Emulator"). It gets access to the
    --   wallet contract handles, the current model state, and the action to be performed.
    perform :: HandleFun state  -- ^ Function from `HandleKey` to `ContractHandle`
            -> ModelState state -- ^ The model state before peforming the action
            -> Action state     -- ^ The action to perform
            -> EmulatorTrace ()
    perform _ _ _ = return ()

    -- | When a test involving random sequences of actions fails, the framework tries to find a
    --   minimal failing test case by shrinking the original failure. Action sequences are shrunk by
    --   removing individual actions, or by replacing an action by one of the (simpler) actions
    --   returned by `shrinkAction`.
    --
    --   See `Test.QuickCheck.shrink` for more information on shrinking.
    shrinkAction :: ModelState state -> Action state -> [Action state]
    shrinkAction _ _ = []

    -- | The `monitoring` function allows you to collect statistics of your testing using QuickCheck
    --   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`,
    --   `Test.QuickCheck.classify`, and `Test.QuickCheck.tabulate`. Statistics on which actions are
    --   executed are always collected.
    monitoring :: (ModelState state, ModelState state)  -- ^ Model state before and after the action
               -> Action state                          -- ^ The action that was performed
               -> Property -> Property
    monitoring _ _ = id

    -- | In some scenarios it's useful to have actions that are never generated randomly, but only
    --   used explicitly in `DL` script `action`s. To avoid these actions matching an `anyAction`
    --   when shrinking, they can be marked `restricted`.
    restricted :: Action state -> Bool
    restricted _ = False

-- | Model state lens
makeLensesFor [("_contractState", "contractStateL")] 'ModelState
makeLensesFor [("_currentSlot",   "currentSlotL")]   'ModelState
makeLensesFor [("_lastSlot",      "lastSlotL")]      'ModelState
makeLensesFor [("_balances",      "balancesL")]      'ModelState
makeLensesFor [("_forged",        "forgedL")]        'ModelState

contractState :: Getter (ModelState state) state
contractState = contractStateL

currentSlot :: Getter (ModelState state) Slot
currentSlot = currentSlotL

balances :: Getter (ModelState state) (Map Wallet Value)
balances = balancesL

forged :: Getter (ModelState state) Value
forged = forgedL

class Monad m => GetModelState m where
    type StateType m :: *
    getModelState :: m (ModelState (StateType m))

getContractState :: GetModelState m => m (StateType m)
getContractState = _contractState <$> getModelState

viewModelState :: GetModelState m => Getting a (ModelState (StateType m)) a -> m a
viewModelState l = (^. l) <$> getModelState

viewContractState :: GetModelState m => Getting a (StateType m) a -> m a
viewContractState l = viewModelState (contractStateL . l)

-- $specMonad
--
-- Stuff about spec monad

runSpec :: Spec state () -> ModelState state -> ModelState state
runSpec (Spec spec) s = Eff.run $ execState s spec

wait :: forall state. Integer -> Spec state ()
wait n = Spec $ modify @(ModelState state) $ over currentSlotL (+ Slot n)

waitUntil :: forall state. Slot -> Spec state ()
waitUntil n = Spec $ modify @(ModelState state) $ over currentSlotL (max n)

forge :: forall s. Value -> Spec s ()
forge v = Spec $ modify @(ModelState s) $ over forgedL (<> v)

burn :: forall s. Value -> Spec s ()
burn = forge . inv

deposit :: forall s. Wallet -> Value -> Spec s ()
deposit w val = Spec $ modify @(ModelState s) (over (balancesL . at w) (Just . maybe val (<> val)))

withdraw :: Wallet -> Value -> Spec s ()
withdraw w val = deposit w (inv val)

transfer :: Wallet -> Wallet -> Value -> Spec s ()
transfer fromW toW val = withdraw fromW val >> deposit toW val

($=) :: Setter' s a -> a -> Spec s ()
l $= x = l $~ const x

($~) :: forall s a. Setter' s a -> (a -> a) -> Spec s ()
l $~ f = Spec $ modify @(ModelState s) (over (contractStateL . l) f)

instance GetModelState (Spec s) where
    type StateType (Spec s) = s
    getModelState = Spec get

handle :: (ContractModel s) => Handles s -> HandleFun s
handle handles key =
    case imLookup key handles of
        Just h  -> h
        Nothing -> error $ "handle: No handle for " ++ show key

-- | Are there locked funds?
lockedFunds :: ModelState s -> Value
lockedFunds s = s ^. forged <> inv (fold $ s ^. balances)

newtype EmulatorAction state = EmulatorAction { runEmulatorAction :: Handles state -> EmulatorTrace (Handles state) }

instance Semigroup (EmulatorAction state) where
    EmulatorAction f <> EmulatorAction g = EmulatorAction (f >=> g)

instance Monoid (EmulatorAction state) where
    mempty  = EmulatorAction pure
    mappend = (<>)

type ContractMonad state = Writer (EmulatorAction state)

runEmulator :: (Handles state -> EmulatorTrace ()) -> ContractMonad state ()
runEmulator a = tell (EmulatorAction $ \ h -> h <$ a h)

getHandles :: EmulatorTrace (Handles state) -> ContractMonad state ()
getHandles a = tell (EmulatorAction $ \ _ -> a)

instance ContractModel state => Show (StateModel.Action (ModelState state) a) where
    showsPrec p (ContractAction a) = showsPrec p a

deriving instance ContractModel state => Eq (StateModel.Action (ModelState state) a)

instance ContractModel state => StateModel (ModelState state) where

    data Action (ModelState state) a = ContractAction (Action state)

    type ActionMonad (ModelState state) = ContractMonad state

    arbitraryAction s = do
        a <- arbitraryAction s
        return (Some @() (ContractAction a))

    shrinkAction s (ContractAction a) = [ Some @() (ContractAction a') | a' <- shrinkAction s a ]

    initialState = ModelState { _currentSlot   = 0
                              , _lastSlot      = 125        -- Set by propRunScript
                              , _balances      = Map.empty
                              , _forged        = mempty
                              , _contractState = initialState }

    nextState s (ContractAction cmd) _v = runSpec (nextState cmd) s

    precondition s (ContractAction cmd) = s ^. currentSlot < s ^. lastSlotL - 10 -- No commands if < 10 slots left
                                          && precondition s cmd

    perform s (ContractAction cmd) _env = error "unused" <$ runEmulator (\ h -> perform (handle h) s cmd)

    postcondition _s _cmd _env _res = True

    monitoring (s0, s1) (ContractAction cmd) _env _res = monitoring (s0, s1) cmd

-- $dynamicLogic
--
-- Stuff about dynamic logic

type DL s = DL.DL (ModelState s)

action :: ContractModel s => Action s -> DL s ()
action cmd = DL.action (ContractAction @_ @() cmd)

assertModel :: String -> (ModelState s -> Bool) -> DL s ()
assertModel = DL.assertModel

forAllDL :: (ContractModel s, Testable p) => DL s () -> (Script s -> p) -> Property
forAllDL = DL.forAllDL

instance ContractModel s => DL.DynLogicModel (ModelState s) where
    restricted (ContractAction act) = restricted act

instance GetModelState (DL s) where
    type StateType (DL s) = s
    getModelState = DL.getModelStateDL

-- $quantify
--
-- Quantify stuff


-- $runningProperties
--
-- Stuff about running properties

runTr :: CheckOptions -> TracePredicate -> ContractMonad state Property -> Property
runTr opts predicate trace =
  flip runCont (const prop) $
    checkPredicateInner opts predicate (void $ runEmulatorAction tr IMNil)
                        debugOutput assertResult
  where
    (prop, tr) = runWriter trace
    debugOutput :: String -> Cont Property ()
    debugOutput out = cont $ \ k -> whenFail (putStrLn out) $ k ()

    assertResult :: Bool -> Cont Property ()
    assertResult ok = cont $ \ k -> ok QC..&&. k ()

activateWallets :: forall state. ContractModel state => [HandleSpec state] -> EmulatorTrace (Handles state)
activateWallets [] = return IMNil
activateWallets (HandleSpec key wallet contract : spec) = do
    h <- activateContractWallet wallet contract
    m <- activateWallets spec
    return $ IMCons key h m

propRunScript_ :: forall state.
    ContractModel state =>
    [HandleSpec state] ->
    Script state ->
    Property
propRunScript_ handleSpecs script =
    propRunScript handleSpecs
                  (\ _ -> pure True)
                  (\ _ _ -> pure ())
                  script
                  (\ _ -> pure ())

propRunScript :: forall state.
    ContractModel state =>
    [HandleSpec state] ->
    (ModelState state -> TracePredicate) ->
    (HandleFun state -> ModelState state -> EmulatorTrace ()) ->
    Script state ->
    (ModelState state -> PropertyM (ContractMonad state) ()) ->
    Property
propRunScript = propRunScriptWithOptions (set minLogLevel Warning defaultCheckOptions)

propRunScriptWithOptions :: forall state.
    ContractModel state =>
    CheckOptions ->
    [HandleSpec state] ->
    (ModelState state -> TracePredicate) ->
    (HandleFun state -> ModelState state -> EmulatorTrace ()) ->
    Script state ->
    (ModelState state -> PropertyM (ContractMonad state) ()) ->
    Property
propRunScriptWithOptions opts handleSpecs predicate before script after =
    monadic (runTr opts finalPredicate) $ do
        QC.run $ getHandles $ activateWallets @state handleSpecs
        let initState = StateModel.initialState { _lastSlot      = opts ^. maxSlot }
        QC.run $ runEmulator $ \ h -> before (handle h) initState
        (st, _) <- runScriptInState initState script
        after st
    where
        finalState     = stateAfter script
        finalPredicate = predicate finalState .&&. checkBalances finalState

checkBalances :: ModelState state -> TracePredicate
checkBalances s = Map.foldrWithKey (\ w val p -> walletFundsChange w val .&&. p) (pure True) (s ^. balances)


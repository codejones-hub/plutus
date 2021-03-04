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
{-# LANGUAGE ConstraintKinds            #-}
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
    , balance
    , forged
    , lockedValue
    , GetModelState(..)
    , getContractState
    , askModelState
    , askContractState
    , viewModelState
    , viewContractState
    -- ** The Spec monad
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
    , modifyContractState
    , ($=)
    , ($~)
    -- * Test scenarios
    --
    -- $dynamicLogic
    , DL
    , action
    , anyAction
    , anyActions
    , anyActions_

    -- ** Failures
    --
    -- $dynamicLogic_errors
    , DL.assert
    , assertModel
    , stopping
    , weight
    , monitor

    -- ** Random generation
    --
    -- $quantify
    , DL.forAllQ
    , module Language.Plutus.Contract.Test.DynamicLogic.Quantify

    -- * Properties
    --
    -- $runningProperties
    , Script
    -- ** Wallet contract handles
    --
    -- $walletHandles
    , SchemaConstraints
    , HandleSpec(..)
    , HandleFun
    -- ** Emulator properties
    , propRunScript_
    , propRunScript
    , propRunScriptWithOptions
    -- ** DL properties
    , forAllDL
    ) where

import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Freer                                 as Eff
import           Control.Monad.Freer.State
import qualified Control.Monad.State                                 as State
import qualified Data.Aeson                                          as JSON
import           Data.Foldable
import           Data.Map                                            (Map)
import qualified Data.Map                                            as Map
import           Data.Row                                            (Row)
import           Data.Typeable

import           Language.Plutus.Contract                            (Contract, HasBlockchainActions)
import           Language.Plutus.Contract.Test
import qualified Language.Plutus.Contract.Test.DynamicLogic.Monad    as DL
import           Language.Plutus.Contract.Test.DynamicLogic.Quantify (Quantifiable (..), Quantification, arbitraryQ,
                                                                      chooseQ, elementsQ, exactlyQ, frequencyQ, mapQ,
                                                                      oneofQ, whereQ)
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
import           Test.QuickCheck.Monadic                             as QC (PropertyM, monadic)
import qualified Test.QuickCheck.Monadic                             as QC

data IMap (key :: i -> j -> *) (val :: i -> j -> *) where
    IMNil  :: IMap key val
    IMCons :: (Typeable i, Typeable j) => key i j -> val i j -> IMap key val -> IMap key val

imLookup :: (Typeable i, Typeable j, Typeable key, Typeable val, Eq (key i j)) => key i j -> IMap key val -> Maybe (val i j)
imLookup _ IMNil = Nothing
imLookup k (IMCons key val m) =
    case cast (key, val) of
        Just (key', val') | key' == k -> Just val'
        _                             -> imLookup k m

-- $walletHandles
--
-- In order to call contract endpoints using `Plutus.Trace.Emulator.callEndpoint`, a `ContractHandle`
-- is required. Contract handles are managed behind the scenes by the `propRunScript` functions,
-- based on a given a list of `HandleSpec`s, associating `HandleKey`s with `Wallet`s and
-- `Contract`s. Before testing starts, `activateContractWallet` is called for all entries in the
-- list and the mapping from `HandleKey` to `ContractHandle` is provided in the `HandleFun` argument
-- to `perform`.

-- | The constraints required on contract schemas and error types to enable calling contract
--   endpoints (`Plutus.Trace.Emulator.callEndpoint`).
type SchemaConstraints schema err =
        ( Typeable schema
        , HasBlockchainActions schema
        , ContractConstraints schema
        , Show err
        , Typeable err
        , JSON.ToJSON err
        , JSON.FromJSON err
        )

-- | A `HandleSpec` associates a `HandleKey` with a concrete `Wallet` and `Contract`. The contract
--   schema and error types are hidden from the outside.
data HandleSpec state where
    HandleSpec :: SchemaConstraints schema err
                  => HandleKey state schema err -- ^ The handle key used when looking up handles in `perform`
                  -> Wallet                     -- ^ The wallet who owns the handle
                  -> Contract schema err ()     -- ^ The contract that is the target of the handle
                  -> HandleSpec state

type Handles state = IMap (HandleKey state) ContractHandle


-- | A function returning the `ContractHandle` corresponding to a `HandleKey`. A `HandleFun` is
--   provided to the `perform` function to enable calling contract endpoints with
--   `Plutus.Trace.Emulator.callEndpoint`.
type HandleFun state = forall schema err. (Typeable schema, Typeable err) => HandleKey state schema err -> ContractHandle schema err

-- | The `ModelState` models the state of the blockchain. It contains,
--
--   * the contract-specific state (`contractState`)
--   * the current slot (`currentSlot`)
--   * the wallet balances (`balances`)
--   * the amount that has been forged (`forged`)
data ModelState state = ModelState
        { _currentSlot   :: Slot
        , _lastSlot      :: Slot
        , _balances      :: Map Wallet Value
        , _forged        :: Value
        , _contractState :: state
        }

-- | A `Script` is essentially a list of `Action`s.
type Script s = StateModel.Script (ModelState s)

instance Show state => Show (ModelState state) where
    show = show . _contractState   -- for now

-- | The `Spec` monad is a state monad over the `ModelState`. It is used exclusively by the
--   `nextState` function to model the effects of an action on the blockchain.
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
    --   their wallets and contracts (a `HandleSpec`). Handle keys are indexed by the schema and
    --   error type of the contract and should be defined as a GADT. For example, a handle type for
    --   a contract with one seller and multiple buyers could look like this.
    --
    --   >  data HandleKey MyModel s e where
    --   >      Buyer  :: Wallet -> HandleKey MyModel MySchema MyError
    --   >      Seller :: HandleKey MyModel MySchema MyError
    data HandleKey state :: Row * -> * -> *

    -- | Given the current model state, provide a QuickCheck generator for a random next action.
    --   This is used in the `Arbitrary` instance for `Script`s as well as by `DL.anyAction` and
    --   `DL.anyActions`.
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
    --   `Test.QuickCheck.classify`, and `Test.QuickCheck.tabulate`. This function is called by
    --   `propRunScript` (and friends) for any actions in the given `Script`.
    --
    --   Statistics on which actions are executed are always collected.
    monitoring :: (ModelState state, ModelState state)  -- ^ Model state before and after the action
               -> Action state                          -- ^ The action that was performed
               -> Property -> Property
    monitoring _ _ = id

    -- | In some scenarios it's useful to have actions that are never generated randomly, but only
    --   used explicitly in `DL` script `action`s. To avoid these actions matching an `DL.anyAction`
    --   when shrinking, they can be marked `restricted`.
    restricted :: Action state -> Bool
    restricted _ = False

-- | Model state lens
makeLensesFor [("_contractState", "contractStateL")] 'ModelState
makeLensesFor [("_currentSlot",   "currentSlotL")]   'ModelState
makeLensesFor [("_lastSlot",      "lastSlotL")]      'ModelState
makeLensesFor [("_balances",      "balancesL")]      'ModelState
makeLensesFor [("_forged",        "forgedL")]        'ModelState

-- | Get the contract-specific part of the model state.
--
--   `Spec` monad update functions: `$=` and `$~`.
contractState :: Getter (ModelState state) state
contractState = contractStateL

-- | Get the current slot.
--
--   `Spec` monad update functions: `wait` and `waitUntil`.
currentSlot :: Getter (ModelState state) Slot
currentSlot = currentSlotL

-- | Get the current wallet balances. These are delta balances, so they start out at zero and can be
--   negative. The absolute balances used by the emulator can be set in the `CheckOptions` argument
--   to `propRunScriptWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balances :: Getter (ModelState state) (Map Wallet Value)
balances = balancesL

-- | Get the current balance for a wallet. This is the delta balance, so it starts out at zero and
--   can be negative. The absolute balance used by the emulator can be set in the `CheckOptions`
--   argument to `propRunScriptWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balance :: Wallet -> Getter (ModelState state) Value
balance w = balancesL . at w . non mempty

-- | Get the amount of tokens forged so far. This is used to compute `lockedValue`.
--
--   `Spec` monad update functions: `forge` and `burn`.
forged :: Getter (ModelState state) Value
forged = forgedL

-- | How much value is currently locked by contracts. This computed by subtracting the wallet
--   `balances` from the `forged` value.
lockedValue :: ModelState s -> Value
lockedValue s = s ^. forged <> inv (fold $ s ^. balances)

-- | Monads with read access to the model state: the `Spec` monad used in `nextState`, and the `DL`
--   monad used to construct test scenarios.
class Monad m => GetModelState m where
    -- | The contract state type of the monad. For both `Spec` and `DL` this is simply the @state@
    --   parameter of the respective monad.
    type StateType m :: *

    -- | Get the current model state.
    getModelState :: m (ModelState (StateType m))

-- | Get the contract state part of the model state.
getContractState :: GetModelState m => m (StateType m)
getContractState = _contractState <$> getModelState

-- | Get a component of the model state.
askModelState :: GetModelState m => (ModelState (StateType m) -> a) -> m a
askModelState f = f <$> getModelState

-- | Get a component of the contract state.
askContractState :: GetModelState m => (StateType m -> a) -> m a
askContractState f = askModelState (f . _contractState)

-- | Get a component of the model state using a lens.
viewModelState :: GetModelState m => Getting a (ModelState (StateType m)) a -> m a
viewModelState l = askModelState (^. l)

-- | Get a component of the contract state using a lens.
viewContractState :: GetModelState m => Getting a (StateType m) a -> m a
viewContractState l = viewModelState (contractStateL . l)

-- $specMonad
--
-- The `Spec` monad is used in the `nextState` function to specify how the model state is affected
-- by each action.
--
-- Note that the model state does not track the absolute `balances` of each wallet, only how the
-- balance changes over the execution of a contract. Thus, token transfers (using `transfer`,
-- `deposit` or `withdraw`) always succeed in the model, but might fail when running the
-- contract in the emulator, causing test failures. The simplest way to deal with this is
-- to make sure that each wallet has enough starting funds to cover any scenario encountered during
-- testing. The starting funds can be provided in the `CheckOptions` argument to
-- `propRunScriptWithOptions`.
-- Another option is to model the starting funds of each contract in the contract state and check
-- that enough funds are available before performing a transfer.

runSpec :: Spec state () -> ModelState state -> ModelState state
runSpec (Spec spec) s = Eff.run $ execState s spec

modState :: forall state a. Setter' (ModelState state) a -> (a -> a) -> Spec state ()
modState l f = Spec $ modify @(ModelState state) $ over l f

-- | Wait the given number of slots. Updates the `currentSlot` of the model state.
wait :: Integer -> Spec state ()
wait n = modState currentSlotL (+ Slot n)

-- | Wait until the given slot. Has no effect if `currentSlot` is greater than the given slot.
waitUntil :: Slot -> Spec state ()
waitUntil n = modState currentSlotL (max n)

-- | Forge tokens. Forged tokens start out as `lockedValue` (i.e. owned by the contract) and can be
--   transferred to wallets using `deposit`.
forge :: Value -> Spec state ()
forge v = modState forgedL (<> v)

-- | Burn tokens. Equivalent to @`forge` . `inv`@.
burn :: Value -> Spec state ()
burn = forge . inv

-- | Add tokens to the `balance` of a wallet. The added tokens are subtracted from the `lockedValue`
--   of tokens held by contracts.
deposit :: Wallet -> Value -> Spec state ()
deposit w val = modState (balancesL . at w) (Just . maybe val (<> val))

-- | Withdraw tokens from a wallet. The withdrawn tokens are added to the `lockedValue` of tokens
--   held by contracts.
withdraw :: Wallet -> Value -> Spec state ()
withdraw w val = deposit w (inv val)

-- | Transfer tokens between wallets, updating their `balances`.
transfer :: Wallet  -- ^ Transfer from this wallet
         -> Wallet  -- ^ to this wallet
         -> Value   -- ^ this many tokens
         -> Spec state ()
transfer fromW toW val = withdraw fromW val >> deposit toW val

-- | Modify the contract state.
modifyContractState :: (state -> state) -> Spec state ()
modifyContractState f = modState contractStateL f

-- | Set a specific field of the contract state.
($=) :: Setter' state a -> a -> Spec state ()
l $= x = l $~ const x

-- | Modify a specific field of the contract state.
($~) :: Setter' state a -> (a -> a) -> Spec state ()
l $~ f = modState (contractStateL . l) f

instance GetModelState (Spec state) where
    type StateType (Spec state) = state
    getModelState = Spec get

handle :: (ContractModel s) => Handles s -> HandleFun s
handle handles key =
    case imLookup key handles of
        Just h  -> h
        Nothing -> error $ "handle: No handle for " ++ show key

newtype EmulatorAction state = EmulatorAction { runEmulatorAction :: Handles state -> EmulatorTrace (Handles state) }

instance Semigroup (EmulatorAction state) where
    EmulatorAction f <> EmulatorAction g = EmulatorAction (f >=> g)

instance Monoid (EmulatorAction state) where
    mempty  = EmulatorAction pure
    mappend = (<>)

type ContractMonad state = State.State (EmulatorAction state)

runEmulator :: (Handles state -> EmulatorTrace ()) -> ContractMonad state ()
runEmulator a = State.modify (<> EmulatorAction (\ h -> h <$ a h))

getHandles :: EmulatorTrace (Handles state) -> ContractMonad state ()
getHandles a = State.modify (<> EmulatorAction (\ _ -> a))

instance ContractModel state => Show (StateModel.Action (ModelState state) a) where
    showsPrec p (ContractAction a) = showsPrec p a

deriving instance ContractModel state => Eq (StateModel.Action (ModelState state) a)

instance ContractModel state => StateModel (ModelState state) where

    data Action (ModelState state) a where
        ContractAction :: Action state -> StateModel.Action (ModelState state) ()

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

    perform s (ContractAction cmd) _env = () <$ runEmulator (\ h -> perform (handle h) s cmd)

    postcondition _s _cmd _env _res = True

    monitoring (s0, s1) (ContractAction cmd) _env _res = monitoring (s0, s1) cmd

-- $dynamicLogic
--
-- Test scenarios are described in the `DL` monad (based on dynamic logic) which lets you freely mix
-- random sequences of actions (`DL.anyAction`, `DL.anyActions_`, `DL.anyActions`) with specific
-- actions (`action`). It also supports checking properties of the model state (`DL.assert`,
-- `assertModel`), and random generation (`DL.forAllQ`).
--
-- For instance, a unit test for a simple auction contract might look something like this:
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      `DL.action` $ Bid w1 100
--      `DL.action` $ Bid w2 150
--      `DL.action` $ Wait endSlot
--      `DL.action` $ Collect
-- @
--
--  and could easily be extended with some randomly generated values
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      bid <- `forAllQ` $ `chooseQ` (1, 100)
--      `DL.action` $ Bid w1 bid
--      `DL.action` $ Bid w2 (bid + 50)
--      `DL.action` $ Wait endSlot
--      `DL.action` $ Collect
-- @
--
-- More interesting scenarios can be constructed by mixing random and fixed sequences. The following
-- checks that you can always finish an auction after which point there are no funds locked by the
-- contract:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- finishAuction = do
--   `DL.anyActions_`
--   `action` $ Wait endSlot
--   `action` $ Collect
--   `assertModel` "Funds are locked!" (`Ledger.Value.isZero` . `lockedValue`)
-- @
--
-- `DL` scripts are turned into QuickCheck properties using `forAllDL`.

-- $dynamicLogic_errors
--
-- In addition to failing the check that the emulator run matches the model, there are a few other
-- ways that test scenarios can fail:
--
-- * an explicit `action` does not satisfy its `precondition`
-- * a failed `DL.assert` or `assertModel`, or a monad `fail`
-- * an `Control.Applicative.empty` set of `Control.Applicative.Alternative`s
-- * the scenario fails to terminate (see `stopping`)
--
-- All of these occur at test case generation time, and thus do not directly say anything about the
-- contract implementation. However, together with the check that the model agrees with the emulator
-- they indirectly imply properties of the implementation. An advantage of this is that `DL` test
-- scenarios can be checked without running the contract through the emulator, which is much much
-- faster. For instance,
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
-- would check that the model does not think there will be any locked funds after the auction is
-- finished. Once this property passes, one can run the slower property that also checks that the
-- emulator agrees.

-- | The monad for writing test scenarios. It supports non-deterministic choice through
--   `Control.Applicative.Alternative`, failure with `MonadFail`, and access to the model state
--   through `GetModelState`. It is lazy, so scenarios can be potentially infinite, although the
--   probability of termination needs to be high enough that concrete test cases are always finite.
--   See `stopping` for more information on termination.
type DL state = DL.DL (ModelState state)

-- | Generate a specific action. Fails if the action's `precondition` is not satisfied.
action :: ContractModel state => Action state -> DL state ()
action cmd = DL.action (ContractAction cmd)

-- | Generate a random action using `arbitraryAction`. The generated action is guaranteed to satisfy
--   its `precondition`. Fails with `DL.Stuck` if no action satisfying the precondition can be found
--   after 100 attempts.
anyAction :: DL state ()
anyAction = DL.anyAction

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. The argument is the expected number of actions in the sequence chosen from a
--   geometric distribution, unless in the `stopping` stage, in which case as few actions as
--   possible are generated.
anyActions :: Int -> DL state ()
anyActions = DL.anyActions

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. Actions are generated until the `stopping` stage is reached.
anyActions_ :: DL state ()
anyActions_ = DL.anyActions_

-- | Test case generation from `DL` scenarios have a target length of the action sequence to be
--   generated that is based on the QuickCheck size parameter (see `sized`). However, given that
--   scenarios can contain explicit `action`s it might not be possible to stop the scenario once the
--   target length has been reached.
--
--   Instead, once the target number of actions have been reached, generation goes into the
--   /stopping/ phase. In this phase branches starting with an `anyAction` or `DL.forAllQ` are
--   avoided if possible. Conversely, before the stopping phase, branches starting with `stopping`
--   are avoided unless there are no other possible choices.
--
--   For example, here is the definition of `anyActions_`:
--
-- @
-- `anyActions_` = `stopping` `Control.Applicative.<|>` (`anyAction` >> `anyActions_`)
-- @
--
--   The effect of this definition is that the second branch will be taken until the desired number
--   of actions have been generated, at which point the `stopping` branch will be taken and
--   generation stops (or continues with whatever comes after the `anyActions_` call).
--
--   Now, it might not be possible, or too hard, to find a way to terminate a scenario. For
--   instance, this scenario has no finite test cases:
--
-- @
-- looping = `anyAction` >> looping
-- @
--
--   To prevent test case generation from looping, if a scenario has not terminated after generating
--   @2 * n + 20@ actions, where @n@ is when the stopping phase kicks in, generation fails with a
--   `DL.Looping` error.
stopping :: DL state ()
stopping = DL.stopping

-- | By default, `Control.Applicative.Alternative` choice (`Control.Applicative.<|>`) picks the two
--   branches with equal probability. To change this you can use `weight`, which multiplies the
--   relative probability of picking a branch by the given number.
--
--   For instance, the following scenario picks the action @a@ with probability @2/3@ and the action
--   @b@ with probability @1/3@:
--
-- @
-- biasedChoice a b = `weight` 2 (`action` a) `Control.Applicative.<|>` `weight` (`action` b)
-- @
--
--   Calls to `weight` need to appear at the top-level after a choice, preceding any actions
--   (`action`/`anyAction`) or random generation (`forAllQ`), or they will have no effect.
weight :: Double -> DL state ()
weight = DL.weight

-- | The `monitor` function allows you to collect statistics of your testing using QuickCheck
--   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`, `Test.QuickCheck.classify`,
--   and `Test.QuickCheck.tabulate`. See also the `monitoring` method of `ContractModel` which is
--   called for all actions in a test case (regardless of whether they are generated by an explicit
--   `action` or an `anyAction`).
monitor :: (Property -> Property) -> DL state ()
monitor = DL.monitorDL

-- | Fail unless the given predicate holds of the model state.
--
--   Equivalent to
--
-- @
-- assertModel msg p = do
--   s <- `getModelState`
--   `DL.assert` msg (p s)
-- @
assertModel :: String -> (ModelState state -> Bool) -> DL state ()
assertModel = DL.assertModel

-- | Turn a `DL` scenario into a QuickCheck property. Generates a random `Script` matching the
--   scenario and feeds it to the given property. The property can be a full property running the
--   emulator and checking the results, defined using `propRunScript_`, `propRunScript`, or
--   `propRunScriptWithOptions`. Assuming a model for an auction contract and `DL` scenario that
--   checks that you can always complete the auction, you can write:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- prop_Auction  = `propRunScript_` handles
--   where handles = ...
-- prop_Finish = `forAllDL` finishAuction prop_Auction
-- @
--
--   However, there is also value in a property that does not run the emulator at all:
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
--   This will check all the assertions and other failure conditions of the `DL` scenario very
--   quickly. Once this property passes a large number of tests, you can run the full property
--   checking that the model agrees with reality.
forAllDL :: (ContractModel state, Testable p) => DL state () -> (Script state -> p) -> Property
forAllDL = DL.forAllDL

instance ContractModel s => DL.DynLogicModel (ModelState s) where
    restricted (ContractAction act) = restricted act

instance GetModelState (DL state) where
    type StateType (DL state) = state
    getModelState = DL.getModelStateDL

-- $quantify
--
-- `DL` scenarios support random generation using `DL.forAllQ`. This does not take a normal
-- QuickCheck `Gen` generator, but a `DL.Quantification`, which aside from a generator also keeps
-- track of which values can be generated. This means test cases coming from scenarios containing
-- `DL.forAll` can be prevented from shrinking to something that could not have been generated in
-- the first place.


-- $runningProperties
--
-- Once you have a `ContractModel` and some `DL` scenarios you need to turn these into QuickCheck
-- properties that can be run by `quickCheck`. The functions `propRunScript_`, `propRunScript`, and
-- `propRunScriptWithOptions` take a sequence of actions (a `Script`), runs it through the
-- blockchain emulator ("Plutus.Trace.Emulator") and checks that the model and the emulator agrees
-- on who owns what tokens at the end.
--
-- To generate a `Script` you can use the `Arbitrary` instance, which generates a random sequence of
-- actions using `arbitraryAction`, or you can use `forAllDL` to generate a `Script` from a `DL`
-- scenario.

finalChecks :: CheckOptions -> TracePredicate -> PropertyM (ContractMonad state) a -> PropertyM (ContractMonad state) a
finalChecks opts predicate prop = do
    x  <- prop
    tr <- QC.run State.get
    x <$ checkPredicateInner opts predicate (void $ runEmulatorAction tr IMNil)
                             debugOutput assertResult
    where
        debugOutput :: Monad m => String -> PropertyM m ()
        debugOutput = QC.monitor . whenFail . putStrLn

        assertResult :: Monad m => Bool -> PropertyM m ()
        assertResult = QC.assert

activateWallets :: forall state. ContractModel state => [HandleSpec state] -> EmulatorTrace (Handles state)
activateWallets [] = return IMNil
activateWallets (HandleSpec key wallet contract : spec) = do
    h <- activateContractWallet wallet contract
    m <- activateWallets spec
    return $ IMCons key h m

-- | Run a `Script` in the emulator and check that the model and the emulator agree on the final
--   wallet balances. Equivalent to
--
-- @
-- propRunScript_ hs script = `propRunScript` hs (`const` `$` `pure` `True`) script
-- @
propRunScript_ ::
    ContractModel state
    => [HandleSpec state]   -- ^ Required wallet contract handles
    -> Script state         -- ^ The script to run
    -> Property
propRunScript_ handleSpecs script =
    propRunScript handleSpecs (\ _ -> pure True) script

-- | Run a `Script` in the emulator and check that the model and the emulator agree on the final
--   wallet balances, and that the given `TracePredicate` holds at the end. Equivalent to:
--
-- @
-- propRunScript = `propRunScriptWithOptions` `defaultCheckOptions`
-- @
propRunScript ::
    ContractModel state
    => [HandleSpec state]                    -- ^ Required wallet contract handles
    -> (ModelState state -> TracePredicate)  -- ^ Predicate to check at the end
    -> Script state                          -- ^ The script to run
    -> Property
propRunScript = propRunScriptWithOptions defaultCheckOptions

-- | Run a `Script` in the emulator and check that the model and the emulator agree on the final
--   wallet balances, and that the given `TracePredicate` holds at the end. The predicate has access
--   to the final model state.
--
--   The `HandleSpec` argument lists the contract instances that should be created for the wallets
--   involved in the test. Before the script is run, contracts are activated using
--   `activateContractWallet` and a mapping from `HandleKey`s to the resulting `ContractHandle`s is
--   provided to the `perform` function.
--
--   The `Script` argument can be generated by a `forAllDL` from a `DL` scenario, or using the
--   `Arbitrary` instance for scripts which generates random actions using `arbitraryAction`:
--
-- >>> quickCheck $ propRunScript_ handles
-- +++ OK, passed 100 tests
-- >>> quickCheck $ forAllDL dl $ propRunScript_ handles
-- +++ OK, passed 100 tests
--
--   The options argument can be used to configure the emulator--setting initial wallet balances,
--   the maximum number of slots to run for, and the log level for the emulator trace printed on
--   failing tests:
--
-- @
-- options :: `Map` `Wallet` `Value` -> `Slot` -> `Control.Monad.Freer.Log.LogLevel` -> `CheckOptions`
-- options dist slot logLevel =
--     `defaultCheckOptions` `&` `emulatorConfig` . `Plutus.Trace.Emulator.initialChainState` `.~` `Left` dist
--                         `&` `maxSlot`                            `.~` slot
--                         `&` `minLogLevel`                        `.~` logLevel
-- @
--
propRunScriptWithOptions ::
    ContractModel state
    => CheckOptions                          -- ^ Emulator options
    -> [HandleSpec state]                    -- ^ Required wallet contract handles
    -> (ModelState state -> TracePredicate)  -- ^ Predicate to check at the end
    -> Script state                          -- ^ The script to run
    -> Property
propRunScriptWithOptions opts handleSpecs predicate script =
    monadic (flip State.evalState mempty) $ finalChecks opts finalPredicate $ do
        QC.run $ getHandles $ activateWallets handleSpecs
        let initState = StateModel.initialState { _lastSlot = opts ^. maxSlot }
        void $ runScriptInState initState script
    where
        finalState     = stateAfter script
        finalPredicate = predicate finalState .&&. checkBalances finalState

checkBalances :: ModelState state -> TracePredicate
checkBalances s = Map.foldrWithKey (\ w val p -> walletFundsChange w val .&&. p) (pure True) (s ^. balances)


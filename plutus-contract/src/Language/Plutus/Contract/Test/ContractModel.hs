-- QuickCheck model library for contracts. Builds on top of
-- Language.Plutus.Contract.Test.StateModel.

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Plutus.Contract.Test.ContractModel
    -- ContractModel
    ( ModelState
    , modelState, currentSlot, balances
    , lockedFunds
    , ContractModel(..)
    , Action(..)
    , addCommands
    , HandleSpec(..)
    , HandleFun
    -- GetModelState
    , GetModelState(..)
    , getModelState
    , viewState
    , viewModelState
    -- Spec monad
    , Spec
    , wait
    , waitUntil
    , forge
    , burn
    , deposit
    , withdraw
    , transfer
    , ($=), ($~)
    -- Dynamic logic
    , DL
    , action
    , DL.anyAction
    , DL.anyActions
    , DL.anyActions_
    , DL.stopping
    , DL.weight
    , DL.getModelStateDL
    , DL.assert
    , DL.assertModel
    , DL.forAllQ
    , DL.forAllDL
    , DL.DynLogic
    , module Language.Plutus.Contract.Test.DynamicLogic.Quantify
    -- Running properties
    , Script
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
import           Language.Plutus.Contract.Test.DynamicLogic.Quantify
import           Language.Plutus.Contract.Test.StateModel            hiding (Script, arbitraryAction, initialState,
                                                                      monitoring, nextState, perform, precondition,
                                                                      shrinkAction)
import qualified Language.Plutus.Contract.Test.StateModel            as StateModel
import           Language.PlutusTx.Monoid                            (inv)
import           Ledger.Slot
import           Ledger.Value                                        (Value)
import           Plutus.Trace.Emulator                               as Trace (ContractHandle, EmulatorTrace,
                                                                               activateContractWallet)

import           Test.QuickCheck                                     hiding ((.&&.))
import qualified Test.QuickCheck                                     as QC
import           Test.QuickCheck.Monadic                             as QC

data IMap (key :: i -> *) (val :: i -> *) where
    IMNil  :: IMap key val
    IMCons :: Typeable i => key i -> val i -> IMap key val -> IMap key val

imLookup :: (Typeable i, Typeable key, Typeable val, Eq (key i)) => key i -> IMap key val -> Maybe (val i)
imLookup _ IMNil = Nothing
imLookup k (IMCons key val m) =
    case cast (key, val) of
        Just (key', val') | key' == k -> Just val'
        _                             -> imLookup k m

data HandleSpec state where
    HandleSpec :: forall state schema.
                  ( Typeable schema
                  , HasBlockchainActions schema
                  , ContractConstraints schema )
                  => HandleKey state schema
                  -> Wallet
                  -> Contract schema (Err state) ()
                  -> HandleSpec state

data HandleVal err schema = HandleVal (ContractHandle schema err)

type Handles state = IMap (HandleKey state) (HandleVal (Err state))

type HandleFun state = forall schema. Typeable schema => HandleKey state schema -> Trace.ContractHandle schema (Err state)

data ModelState state = ModelState
        { _currentSlot :: Slot
        , _lastSlot    :: Slot
        , _balances    :: Map Wallet Value
        , _forged      :: Value
        , _modelState  :: state
        }

type Script s = StateModel.Script (ModelState s)

instance Show state => Show (ModelState state) where
    show = show . _modelState   -- for now

type Spec state = Eff '[State (ModelState state)]

class ( Typeable state
      , Show state
      , Show (Command state)
      , Eq (Command state)
      , Show (Err state)
      , Typeable (Err state)
      , (forall s. Eq (HandleKey state s))
      , (forall s. Show (HandleKey state s))
      , JSON.ToJSON (Err state)
      , JSON.FromJSON (Err state)
      ) => ContractModel state where
    data Command state
    type Err state
    data HandleKey state :: Row * -> *

    arbitraryCommand :: ModelState state -> Gen (Command state)

    initialState :: state

    precondition :: ModelState state -> Command state -> Bool
    precondition _ _ = True

    nextState :: Command state -> Spec state ()
    nextState _ = return ()

    perform :: HandleFun state -> ModelState state -> Command state -> EmulatorTrace ()
    perform _ _ _ = return ()

    monitoring :: (ModelState state, ModelState state) -> Command state -> Property -> Property
    monitoring _ _ = id

    shrinkCommand :: ModelState state -> Command state -> [Command state]
    shrinkCommand _ _ = []

makeLenses 'ModelState

class Monad m => GetModelState m where
    type StateType m :: *
    getState :: m (ModelState (StateType m))

getModelState :: GetModelState m => m (StateType m)
getModelState = _modelState <$> getState

viewState :: GetModelState m => Getting a (ModelState (StateType m)) a -> m a
viewState l = (^. l) <$> getState

viewModelState :: GetModelState m => Getting a (StateType m) a -> m a
viewModelState l = viewState (modelState . l)

runSpec :: Spec state () -> ModelState state -> ModelState state
runSpec spec s = Eff.run $ execState s spec

wait :: forall state. Integer -> Spec state ()
wait n = modify @(ModelState state) $ over currentSlot (+ Slot n)

waitUntil :: forall state. Slot -> Spec state ()
waitUntil n = modify @(ModelState state) $ over currentSlot (max n)

forge :: forall s. Value -> Spec s ()
forge v = modify @(ModelState s) $ over forged (<> v)

burn :: forall s. Value -> Spec s ()
burn = forge . inv

deposit :: forall s. Wallet -> Value -> Spec s ()
deposit w val = modify @(ModelState s) (over (balances . at w) (Just . maybe val (<> val)))

withdraw :: Wallet -> Value -> Spec s ()
withdraw w val = deposit w (inv val)

transfer :: Wallet -> Wallet -> Value -> Spec s ()
transfer fromW toW val = withdraw fromW val >> deposit toW val

($=) :: ASetter s s a b -> b -> Spec s ()
l $= x = l $~ const x

($~) :: forall s a b. ASetter s s a b -> (a -> b) -> Spec s ()
l $~ f = modify @(ModelState s) (over (modelState . l) f)

instance GetModelState (Spec s) where
    type StateType (Spec s) = s
    getState = get

handle :: (ContractModel s) => Handles s -> HandleFun s
handle handles key =
    case imLookup key handles of
        Just (HandleVal h) -> h
        Nothing            -> error $ "handle: No handle for " ++ show key

lockedFunds :: ModelState s -> Value
lockedFunds s = s ^. forged <> inv (fold $ s ^. balances)

addCommands :: forall state. ContractModel state => Script state -> [Command state] -> Script state
addCommands (StateModel.Script s) cmds = StateModel.Script $ s ++ [Var i := ContractAction @state @() cmd | (cmd, i) <- zip cmds [n + 1..] ]
    where
        n = last $ 0 : [ i | Var i := _ <- s ]

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

instance ContractModel state => Show (Action (ModelState state) a) where
    showsPrec p (ContractAction a) = showsPrec p a

deriving instance ContractModel state => Eq (Action (ModelState state) a)

instance ContractModel state => StateModel (ModelState state) where

    data Action (ModelState state) a = ContractAction (Command state)

    type ActionMonad (ModelState state) = ContractMonad state

    arbitraryAction s = do
        a <- arbitraryCommand s
        return (Some @() (ContractAction a))

    shrinkAction s (ContractAction a) = [ Some @() (ContractAction a') | a' <- shrinkCommand s a ]

    initialState = ModelState { _currentSlot   = 0
                              , _lastSlot      = 125        -- Set by propRunScript
                              , _balances      = Map.empty
                              , _forged        = mempty
                              , _modelState    = initialState }

    nextState s (ContractAction cmd) _v = runSpec (nextState cmd) s

    precondition s (ContractAction cmd) = s ^. currentSlot < s ^. lastSlot - 10 -- No commands if < 10 slots left
                                          && precondition s cmd

    perform s (ContractAction cmd) _env = error "unused" <$ runEmulator (\ h -> perform (handle h) s cmd)

    postcondition _s _cmd _env _res = True

    monitoring (s0, s1) (ContractAction cmd) _env _res = monitoring (s0, s1) cmd

-- * Dynamic logic

type DL s = DL.DL (ModelState s)

action :: ContractModel s => Command s -> DL s ()
action cmd = DL.action (ContractAction @_ @() cmd)

instance ContractModel s => DL.DynLogicModel (ModelState s) where
    restricted _ = False

instance GetModelState (DL s) where
    type StateType (DL s) = s
    getState = DL.getModelStateDL

-- * Running the model

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
    return $ IMCons key (HandleVal h) m

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


-- QuickCheck model library for contracts. Builds on top of
-- Language.Plutus.Contract.Test.StateModel.

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Plutus.Contract.Test.ContractModel
    -- * ContractModel
    ( ModelState
    , modelState, currentSlot, balances
    , handle, contractInstanceId
    , ContractModel(..)
    -- * Spec monad
    , Spec
    , wait
    , deposit
    , withdraw
    , transfer
    , ($=), ($~)
    , getState
    , getModelState
    -- * Running properties
    , Script
    , propRunScript
    , propRunScriptWithOptions
    ) where

import           Control.Lens
import           Control.Monad.Cont
import           Control.Monad.Freer                      as Eff
import           Control.Monad.Freer.State
import qualified Data.Aeson                               as JSON
import           Data.Map                                 (Map)
import qualified Data.Map                                 as Map
import           Data.Row                                 (Row)

import           Language.Plutus.Contract                 (Contract, ContractInstanceId, HasBlockchainActions)
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.StateModel hiding (Script, arbitraryAction, initialState, monitoring,
                                                           nextState, perform, precondition, shrinkAction)
import qualified Language.Plutus.Contract.Test.StateModel as StateModel
import           Language.PlutusTx.Monoid                 (inv)
import           Ledger.Value                             (Value)
import           Plutus.Trace.Emulator                    as Trace (ContractHandle, EmulatorTrace,
                                                                    activateContractWallet, chInstanceId)

import           Test.QuickCheck                          hiding ((.&&.))
import qualified Test.QuickCheck                          as QC
import           Test.QuickCheck.Monadic                  as QC

data ModelState state = ModelState
        { _currentSlot   :: Int
        , _balances      :: Map Wallet Value
        , _walletHandles :: Map Wallet (ContractHandle (Schema state) (Err state))
        , _modelState    :: state
        }

type Script s = StateModel.Script (ModelState s)

instance Show state => Show (ModelState state) where
    show = show . _modelState   -- for now

type Handle state = ContractHandle (Schema state) (Err state)

type Spec state = Eff '[State (ModelState state)]

class ( Show (Command state)
      , Show (Err state)
      , JSON.ToJSON (Err state)
      , JSON.FromJSON (Err state)
      ) => ContractModel state where
    data Command state
    type Schema state :: Row *
    type Err state

    arbitraryCommand :: ModelState state -> Gen (Command state)

    initialState :: state

    precondition :: ModelState state -> Command state -> Bool

    nextState :: Command state -> Spec state ()

    perform :: ModelState state -> Command state -> EmulatorTrace ()

    monitoring :: (ModelState state, ModelState state) -> Command state -> Property -> Property

    shrinkCommand :: ModelState state -> Command state -> [Command state]

makeLenses 'ModelState

runSpec :: Spec state () -> ModelState state -> ModelState state
runSpec spec s = Eff.run $ execState s spec

wait :: forall state. Int -> Spec state ()
wait n = modify @(ModelState state) (over currentSlot (+ n))

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

getState :: Getter (ModelState s) a -> Spec s a
getState l = gets (^. l)

getModelState :: Getter s a -> Spec s a
getModelState l = getState (modelState . l)

handle :: ModelState s -> Wallet -> Trace.ContractHandle (Schema s) (Err s)
handle s w = s ^?! walletHandles . at w . _Just

-- Using this function in models makes ghc choke.
-- callEndpoint ::
--     forall l s ep.
--     (ContractConstraints (Schema s), HasEndpoint l ep (Schema s)) => ModelState s -> Wallet -> ep -> EmulatorTrace ()
-- callEndpoint s w v =
--     case s ^. walletHandles . at w of
--         Nothing -> return () -- fail in some way? could add error effect on top of EmulatorTrace
--         Just h  -> Trace.callEndpoint @l @ep @(Schema s) h v

contractInstanceId :: ModelState s -> Wallet -> ContractInstanceId
contractInstanceId s w = chInstanceId $ handle s w

instance ContractModel state => Show (Action (ModelState state) a) where
    showsPrec p (ContractAction a) = showsPrec p a

instance ContractModel state => StateModel (ModelState state) where

    data Action (ModelState state) a = ContractAction (Command state)

    type ActionMonad (ModelState state) = EmulatorTrace

    arbitraryAction s = do
        a <- arbitraryCommand s
        return (Some @() (ContractAction a))

    shrinkAction s (ContractAction a) = [ Some @() (ContractAction a') | a' <- shrinkCommand s a ]

    initialState = ModelState { _currentSlot   = 0
                              , _balances      = Map.empty
                              , _walletHandles = Map.empty
                              , _modelState    = initialState }

    nextState s (ContractAction cmd) _v = runSpec (nextState cmd) s

                                                -- TODO: remember actual maxSlots
    precondition s (ContractAction cmd) = s ^. currentSlot < 100 && precondition s cmd

    perform s (ContractAction cmd) _env = error "unused" <$ perform s cmd

    postcondition _s _cmd _env _res = True

    monitoring (s0, s1) (ContractAction cmd) _env _res = monitoring (s0, s1) cmd

    isFinal _s = False

-- * Running the model

runTr :: CheckOptions -> TracePredicate -> EmulatorTrace () -> Property
runTr opts predicate action =
  flip runCont (const $ property True) $
    checkPredicateInner opts predicate action
                        debugOutput assertResult
  where
    debugOutput :: String -> Cont Property ()
    debugOutput out = cont $ \ k -> whenFail (putStrLn out) $ k ()

    assertResult :: Bool -> Cont Property ()
    assertResult ok = cont $ \ k -> ok QC..&&. k ()

activateWallets :: forall state.
    ( ContractModel state
    , HasBlockchainActions (Schema state)
    , ContractConstraints (Schema state)
    ) => [Wallet] -> Contract (Schema state) (Err state) () -> EmulatorTrace (Map Wallet (Handle state))
activateWallets wallets contract =
    Map.fromList . zip wallets <$> mapM (flip (activateContractWallet @(Schema state)) contract) wallets

propRunScript :: forall state.
    ( HasBlockchainActions (Schema state)
    , ContractConstraints (Schema state)
    , ContractModel state ) =>
    [Wallet] ->
    Contract (Schema state) (Err state) () ->
    (ModelState state -> TracePredicate) ->
    (ModelState state -> EmulatorTrace ()) ->
    Script state ->
    (ModelState state -> PropertyM EmulatorTrace ()) ->
    Property
propRunScript = propRunScriptWithOptions defaultCheckOptions

propRunScriptWithOptions :: forall state.
    ( HasBlockchainActions (Schema state)
    , ContractConstraints (Schema state)
    , ContractModel state ) =>
    CheckOptions ->
    [Wallet] ->
    Contract (Schema state) (Err state) () ->
    (ModelState state -> TracePredicate) ->
    (ModelState state -> EmulatorTrace ()) ->
    Script state ->
    (ModelState state -> PropertyM EmulatorTrace ()) ->
    Property
propRunScriptWithOptions opts wallets contract predicate before script after =
    monadic (runTr opts finalPredicate . void) $ do
        handles <- QC.run $ activateWallets @state wallets contract
        let initState = StateModel.initialState { _walletHandles = handles }
        QC.run $ before initState
        (st, _) <- runScriptInState initState script
        after st
    where
        finalState     = stateAfter script
        finalPredicate = predicate finalState .&&. checkBalances finalState

checkBalances :: ModelState state -> TracePredicate
checkBalances s = Map.foldrWithKey (\ w val p -> walletFundsChange w val .&&. p) (pure True) (s ^. balances)


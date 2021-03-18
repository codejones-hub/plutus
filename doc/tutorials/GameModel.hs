{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module GameStateMachineModel where

import           Control.Lens                                              hiding (elements)
import           Control.Monad
import           Control.Monad.Freer.Log                                   (LogLevel (..))
import           Data.Maybe
import           Test.QuickCheck

import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.ContractModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value
import           Plutus.Trace.Emulator                                     as Trace

-- * QuickCheck model

data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
  deriving (Show)

makeLenses 'GameModel

deriving instance Eq (ContractInstanceKey GameModel schema err)
deriving instance Show (ContractInstanceKey GameModel schema err)

instance ContractModel GameModel where

    data ContractInstanceKey GameModel schema err where
        WalletKey :: Wallet -> ContractInstanceKey GameModel GameStateMachineSchema GameError

    -- The commands available to a test case
    data Action GameModel = Lock      Wallet String Integer
                           | Guess     Wallet String String Integer
                           | GiveToken Wallet
        deriving (Eq, Show)

    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

    -- 'perform' gets a state, which includes the GameModel state, but also contract handles for the
    -- wallets and what the model thinks the current balances are.
    perform handle s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lock" (handle $ WalletKey w)
                         LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> do
            callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs{ guessArgsOldSecret = old
                         , guessArgsNewSecret = new
                         , guessArgsValueTakenOut = Ada.lovelaceValueOf val}
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            payToWallet w w' gameTokenVal
            return ()
            delay 1

    -- 'nextState' descibes how each command affects the state of the model
    nextState (Lock w secret val) = do
        wasUnlocked <- (Nothing ==) <$> viewContractState hasToken
        when wasUnlocked $ do
          hasToken      $= Just w
          currentSecret $= secret
          gameValue     $= val
          forge gameTokenVal
          deposit  w gameTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2

    nextState (Guess w old new val) = do
        correctGuess <- (old ==)    <$> viewContractState currentSecret
        holdsToken   <- (Just w ==) <$> viewContractState hasToken
        enoughAda    <- (val <=)    <$> viewContractState gameValue
        when (correctGuess && holdsToken && enoughAda) $ do
            currentSecret $= new
            gameValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val
        wait 1

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        transfer w0 w gameTokenVal
        hasToken $= Just w
        wait 1

    -- To generate a random test case we need to know how to generate a random
    -- command given the current model state.
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue ] ++
        [ Guess w   <$> genGuess  <*> genGuess <*> choose (0, val)
        | Just w <- [tok] ] ++
        [ GiveToken <$> genWallet ]
        where
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gameValue


    -- The 'precondition' says when a particular command is allowed.
    precondition s cmd = case cmd of
            Lock _ _ v    -> tok == Nothing && v >= 0
            Guess w _ _ v -> tok == Just w && v <= val
            GiveToken w   -> tok /= Nothing
        where
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gameValue



    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkAction _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]

    monitoring (s,_) (Guess w old new v) =
      tabulate "Guesses"
        [if old==s ^. contractState . currentSecret then "Right" else "Wrong"]
    monitoring _ _ = id

handleSpec :: [ContractInstanceSpec GameModel]
handleSpec = [ ContractInstanceSpec (WalletKey w) w G.contract | w <- wallets ]

-- | The main property. 'propRunActions_' checks that balances match the model after each test.
prop_Game :: Actions GameModel -> Property
prop_Game script = propRunActions_ handleSpec script

propGame' :: LogLevel -> Actions GameModel -> Property
propGame' l s = propRunActionsWithOptions
                    (set minLogLevel l defaultCheckOptions)
                    handleSpec
                    (\ _ -> pure True)
                    s

wallets :: [Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen Wallet
genWallet = elements wallets

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = elements guesses

guesses = ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary

delay :: Int -> EmulatorTrace ()
delay n = void $ waitNSlots (fromIntegral n)

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: Property
prop_UnitTest = withMaxSuccess 1 $ forAllDL unitTest prop_Game

unitTest :: DL GameModel ()
unitTest = do
    val <- forAllQ $ chooseQ (3, 20)
    action $ Lock w1 "hello" val
    action $ GiveToken w2
    action $ Guess w2 "hello" "new secret" 3

unitTest2 :: DL GameModel ()
unitTest2 = do
    unitTest
    action $ GiveToken w3
    action $ Guess w3 "new secret" "hello" 4

unitTestFail :: DL GameModel ()
unitTestFail = do
    action $ Lock w1 "hello" 8
    action $ GiveToken w2
    action $ Guess w2 "hola" "new secret" 3

noLockedFunds :: DL GameModel ()
noLockedFunds = do
    (w0,funds,pass) <- forAllQ (elementsQ wallets, chooseQ (1,10000), elementsQ guesses)
    action $ Lock w0 pass funds
    anyActions_
    w <- forAllQ $ elementsQ wallets
    secret <- viewContractState currentSecret
    val    <- viewContractState gameValue
    when (val > 0) $ do
        monitor $ label "Unlocking funds"
        action $ GiveToken w
        action $ Guess w secret "" val
    assertModel "Locked funds should be zero" $ isZero . lockedValue

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = forAllDL noLockedFunds prop_Game

-- | Wallets and game token.

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"

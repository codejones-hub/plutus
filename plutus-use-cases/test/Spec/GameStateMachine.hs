{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.GameStateMachine where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Log                                   (LogLevel (..))
import           Control.Monad.Freer.Writer
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Test.QuickCheck                                           as QC hiding ((.&&.))
import           Test.QuickCheck.Monadic
import           Test.Tasty                                                hiding (after)
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Test                             hiding (not)
import           Language.Plutus.Contract.Test.ContractModel
import           Language.Plutus.Contract.Test.DynamicLogic.Monad
import           Language.Plutus.Contract.Test.StateModel                  (stateAfter)
import qualified Language.Plutus.Contract.Test.StateModel                  as StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value, isZero)
import           Plutus.Trace.Emulator                                     as Trace
import qualified Wallet.Emulator                                           as EM
import           Wallet.Emulator.Folds                                     (postMapM)

-- * QuickCheck model


data GameModel = GameModel
    { _gameValue     :: Integer
    , _keeper        :: Maybe EM.Wallet
    , _hasToken      :: Maybe EM.Wallet
    , _currentSecret :: String
    , _tokenLock     :: Integer
    , _busy          :: Integer }
    deriving (Show)

makeLenses 'GameModel

instance ContractModel GameModel where

    data Command GameModel = Lock      EM.Wallet String Integer
                           | Guess     EM.Wallet String String Integer
                           | GiveToken EM.Wallet
                           | Delay
        deriving (Eq, Show)

    type Schema GameModel = GameStateMachineSchema
    type Err    GameModel = GameError

    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _keeper        = Nothing
        , _currentSecret = ""
        , _tokenLock     = 0
        , _busy          = 0
        }

    precondition s (Lock _ _ _)         = Nothing == s ^. modelState . hasToken
    precondition s (Guess w _old _ val) = and [ Just w == s ^. modelState . hasToken
                                              , val <= s ^. modelState . gameValue
                                              ] -- , Just w /= s ^. modelState . keeper ]
    precondition s (GiveToken w)        = and [ (s ^. modelState . hasToken) `notElem` [Nothing, Just w]
                                              , s ^. modelState . gameValue > 0 ] -- stops the test
    precondition _ Delay                = True

    nextState (Lock w secret val) = do
        hasToken      $= Just w
        keeper        $= Just w
        currentSecret $= secret
        gameValue     $= val
        forge gameTokenVal
        deposit  w gameTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2

    nextState (Guess w old new val) = do
        b <- getModelState busy
        when (b == 0) $ do
            correct <- (old ==) <$> getModelState currentSecret
            if not correct then busyFor 1 0 else do
                busyFor 1 1
                keeper        $= Just w
                currentSecret $= new
                gameValue     $~ subtract val
                deposit w $ Ada.lovelaceValueOf val

    nextState (GiveToken w) = do
        lock <- (> 0) <$> getModelState tokenLock
        lessBusy
        unless lock $ do
            w0 <- fromJust <$> getModelState hasToken
            transfer w0 w gameTokenVal
            hasToken $= Just w
        wait 1

    nextState Delay = do
        lessBusy
        wait 1

    arbitraryCommand s0 = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue
            | Nothing == s ^. hasToken ] ++
        [ Guess w   <$> genGuess <*> genGuess <*> choose (0, s ^. gameValue)
            | Just w <- [s ^. hasToken], s ^. hasToken /= s ^. keeper, s ^. gameValue > 0 ] ++
        [ GiveToken <$> genWallet
            | s ^. hasToken /= Nothing ] ++
        [ return Delay ]
        where s = s0 ^. modelState

    shrinkCommand _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkCommand _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkCommand _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]
    shrinkCommand _s Delay = []

    perform s cmd = case cmd of
        Lock w new val -> do
            callEndpoint @"lock" (handle s w) LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> do
            callEndpoint @"guess" (handle s w) GuessArgs{guessArgsOldSecret = old, guessArgsNewSecret = new, guessArgsValueTakenOut = Ada.lovelaceValueOf val}
        GiveToken w' -> do
            let w = s ^?! modelState . hasToken . _Just
            payToWallet w w' gameTokenVal
            delay 1
        Delay -> delay 1

    monitoring (s0, s1) act =
      case act of
        GiveToken _ | s0 ^. modelState . busy > 0     -> classify True "passing-while-busy"
        Guess _ guess _ _
          | s0 ^. modelState . currentSecret == guess -> classify True "guessing-correctly"
          | otherwise                                 -> classify True "guessing-wrongly"
        _                         -> id
      . (counterexample $ show s1)

lessBusy :: Spec GameModel ()
lessBusy = do
    let dec n = max 0 (n - 1)
    busy      $~ dec
    tokenLock $~ dec

busyFor :: Integer -> Integer -> Spec GameModel ()
busyFor n t = do
    busy      $~ max n
    tokenLock $~ max t

finalPredicate :: ModelState GameModel -> TracePredicate
finalPredicate s = foldr (.&&.) (pure True) $ map notDone wallets
    where
        notDone w = assertNotDone G.contract (walletInstanceTag w) "done"

wallets :: [EM.Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen EM.Wallet
genWallet = QC.elements wallets

shrinkWallet :: EM.Wallet -> [EM.Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = QC.elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getPositive <$> arbitrary

delay :: Int -> EmulatorTrace ()
delay n = void $ waitNSlots (fromIntegral n)

prop_Game :: Script GameModel -> Property
prop_Game = propGame' Info

propGame' :: LogLevel -> Script GameModel -> Property
propGame' l s = propRunScriptWithOptions (set minLogLevel l defaultCheckOptions)
                                                   wallets G.contract finalPredicate before s after
    where
        before _ = return ()
        after  _ = run (delay 10)

type DLC s = DL (ModelState s)

actionC :: ContractModel s => Command s -> DLC s ()
actionC cmd = action (ContractAction @_ @() cmd)

noLockedFunds :: DLC GameModel ()
noLockedFunds = do
    anyActions_
    w <- forAllQ (elementsQ wallets) -- Anyone can get the money.
    s <- getModelStateDL
    let secret = s ^. modelState . currentSecret
        val    = s ^. modelState . gameValue
    unless (isZero $ lockedFunds s) $ do
        actionC $ GiveToken w
        actionC $ Guess w secret secret val
        assertModel $ isZero . lockedFunds

-- Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = forAllDL noLockedFunds prop_Game

-- * Unit tests

tests :: TestTree
tests =
    testGroup "state machine tests"
    [ checkPredicate "run a successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3 <> gameTokenVal)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        successTrace

    , checkPredicate "run a 2nd successful game trace"
        (walletFundsChange w2 (Ada.lovelaceValueOf 3)
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 1 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8))
        .&&. walletFundsChange w3 (Ada.lovelaceValueOf 4 <> gameTokenVal))
        successTrace2

    , checkPredicate "run a failed trace"
        (walletFundsChange w2 gameTokenVal
        .&&. valueAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        failTrace

    , Lib.goldenPir "test/Spec/gameStateMachine.pir" $$(PlutusTx.compile [|| mkValidator ||])

    , HUnit.testCase "script size is reasonable"
        (Lib.reasonable (Scripts.validatorScript G.scriptInstance) 49000)

    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

w1 :: EM.Wallet
w1 = EM.Wallet 1

w2 :: EM.Wallet
w2 = EM.Wallet 2

w3 :: EM.Wallet
w3 = EM.Wallet 3

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a correct guess and locks the remaining
--   funds with a new secret
successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    void $ Trace.waitNSlots 1

-- | Run 'successTrace', then wallet 2 transfers the token to wallet 3, which
--   makes another correct guess
successTrace2 :: EmulatorTrace ()
successTrace2 = do
    successTrace
    _ <- Trace.payToWallet w2 w3 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl3 <- Trace.activateContractWallet w3 G.contract
    Trace.callEndpoint @"guess" hdl3 GuessArgs{guessArgsOldSecret="new secret", guessArgsNewSecret="hello", guessArgsValueTakenOut=Ada.lovelaceValueOf 4}
    void $ Trace.waitNSlots 1


-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a wrong guess
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    _ <- Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hola", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    void $ Trace.waitNSlots 1

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"

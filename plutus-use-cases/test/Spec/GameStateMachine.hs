{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.GameStateMachine where

import           Control.Monad
import           Control.Monad.Freer.Error
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Control.Monad                                             (void)
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value)
import           Plutus.Trace.Emulator                                     (EmulatorTrace)
import qualified Plutus.Trace.Emulator                                     as Trace
import qualified Wallet.Emulator                                           as EM

-- * QuickCheck model

data GameModel = GameModel
    { gameValue     :: Integer
    , keeper        :: Maybe EM.Wallet
    , hasToken      :: Maybe EM.Wallet
    , currentSecret :: String
    , balances      :: Map EM.Wallet Integer
    , tokenLock     :: Integer
    , busy          :: Integer }
    deriving (Show)

instance StateModel GameModel where
    data Action GameModel = Lock EM.Wallet String Integer
                          | Guess EM.Wallet String String Integer
                          | PassToken EM.Wallet EM.Wallet
                          | Delay
        deriving (Show)

    data Ret GameModel = RetOk | RetFail (TraceError G.GameError)
        deriving (Show)

    type ActionMonad GameModel = ContractTrace GameStateMachineSchema G.GameError ()

    initialState = GameModel
        { gameValue     = 0
        , hasToken      = Nothing
        , keeper        = Nothing
        , currentSecret = ""
        , balances      = Map.empty
        , tokenLock     = 0
        , busy          = 0
        }

    isFinal _ = False

    precondition s (Lock _ _ _)        = Nothing == hasToken s
    precondition s (Guess w old _ val) = and [ Just w == hasToken s
                                             , val <= gameValue s
                                             -- , old == currentSecret s
                                             -- , busy s == 0   -- <== precondition to avoid inactive endpoint
                                             , Just w /= keeper s ]
    precondition s (PassToken w w')    = and [ Just w == hasToken s
                                             , w /= w'
                                             -- , busy s == 0
                                             , gameValue s > 0 ] -- stops the test
    precondition _ Delay               = True

    nextState s (Lock w secret val)    _ = s { hasToken      = Just w
                                             , keeper        = Just w
                                             , currentSecret = secret
                                             , gameValue     = val
                                             , balances      = Map.singleton w (-val) }
    nextState s (Guess w old new val) _
        | busy s > 0                     = s
        | old /= currentSecret s         = busyFor 1 0 s
        | otherwise                      = busyFor 1 1 $ s
                                             { keeper        = Just w
                                             , currentSecret = new
                                             , gameValue     = gameValue s - val
                                             -- , balances      = Map.insert w val $ balances s    -- <== BUG
                                             , balances      = Map.insertWith (+) w val $ balances s
                                             }
    nextState s (PassToken _ w) _
      | tokenLock s > 0 = lessBusy s
      | otherwise       = lessBusy $ s { hasToken = Just w }
    nextState s Delay _ = lessBusy s

    postcondition s (Guess w _ _ _) _ (RetFail (HookError (EndpointNotActive (Just w') _)))
      | w == w' = busy s > 0
    postcondition _ _ _ RetOk     = True
    postcondition _ _ _ RetFail{} = False

    arbitraryAction s = oneof $
        [ Lock        <$> genWallet <*> genGuess <*> genValue | Nothing == hasToken s ] ++
        [ Guess w     <$> genGuess <*> genGuess <*> choose (1, gameValue s)
          | Just w <- [hasToken s], hasToken s /= keeper s, gameValue s > 0 ] ++
        [ PassToken w <$> genWallet | Just w <- [hasToken s] ] ++
        [ return Delay ]

    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction _s (PassToken w w') =
        [PassToken w w'' | w'' <- shrinkWallet w']
    shrinkAction _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]
    shrinkAction _s Delay = []

    perform cmd _env = handle $ case cmd of
        Lock w new val -> do
            callEndpoint @"lock" w LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            handleBlockchainEvents w
            addBlocks 1
            handleBlockchainEvents w
            addBlocks 1
        Guess w old new val -> do
            callEndpoint @"guess" w GuessArgs{guessArgsOldSecret = old, guessArgsNewSecret = new, guessArgsValueTakenOut = Ada.lovelaceValueOf val}
        PassToken w w' -> do
            payToWallet w w' gameTokenVal
            delay 1
        Delay -> delay 1
        where
            handle m = catchError (RetOk <$ m) (return . RetFail)

    monitoring (s0, s1) act _env _res =
      case act of
        PassToken _ _ | busy s0 > 0 -> classify True "passing-while-busy"
        _                           -> id
      . (counterexample $ show s1)

lessBusy :: GameModel -> GameModel
lessBusy s = s { busy = 0 `max` (busy s - 1), tokenLock = 0 `max` (tokenLock s - 1) }

busyFor :: Integer -> Integer -> GameModel -> GameModel
busyFor n t s = s { busy = n `max` busy s, tokenLock = t `max` tokenLock s }

finalPredicate :: GameModel -> TracePredicate GameStateMachineSchema (TraceError G.GameError) ()
finalPredicate s = Map.foldrWithKey change top $ balances s
    where
        change w val rest = walletFundsChange w (Ada.lovelaceValueOf val <> gameTok) /\ rest
            where
                gameTok | Just w == hasToken s = gameTokenVal
                        | otherwise            = mempty

wallets :: [EM.Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen EM.Wallet
genWallet = elements wallets

shrinkWallet :: EM.Wallet -> [EM.Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getPositive <$> arbitrary

delay :: Int -> ActionMonad GameModel ()
delay n = do
  replicateM_ n $ do
    mapM_ handleBlockchainEvents wallets
    addBlocks 1
  return ()

prop_Game :: Shrink2 (Script GameModel) -> Property
prop_Game (Shrink2 s) = propRunScript finalPredicate G.contract (return ()) s $ \ _ -> run (delay 10)

-- Generic property to check that we don't get stuck. This only tests the model, but if the model
-- thinks it's not stuck, but the actual implementation is, the property running the contract will
-- fail. Except: this is only true if the precondition does not admit invalid transactions. At the
-- moment the off-chain contract breaks if you try to take an invalid transaction, so this isn't a
-- problem.
prop_notStuck :: StateModel state => Shrink2 (Script state) -> Property
prop_notStuck (Shrink2 s) = notStuck s

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

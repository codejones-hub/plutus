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
import qualified Control.Monad.Freer.State                                 as Eff
import           Control.Monad.Writer
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Data.Row                                                  (Forall)
import qualified Data.Text                                                 as Text
import           Data.Text.Prettyprint.Doc
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Schema                           (Event (..), Handlers (..), Input, Output)
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value)
import qualified Wallet.Emulator                                           as EM

-- * QuickCheck model

type Tr = ContractTrace GameStateMachineSchema G.GameError ()

runTr :: Tr Property -> Property
runTr tr =
    case runTraceWithDistribution defaultDist G.contract tr of
        (Right (p, _), _) -> p
        (Left err, _s)    -> whenFail (print err) False

getEmulatorState :: ContractTrace s e a EM.EmulatorState
getEmulatorState = Eff.get

getContractTraceState :: ContractTrace s e a (ContractTraceState s (TraceError e) a)
getContractTraceState = Eff.get

assertPredicate :: forall s e a.
    ( Show e
    , Forall (Input s) Pretty
    , Forall (Output s) Pretty
    ) => TracePredicate s (TraceError e) a -> PropertyM (ContractTrace s e a) ()
assertPredicate predicate = do
    em <- run getEmulatorState
    st <- run getContractTraceState
    let r = ContractTraceResult em st
        (result, testOutputs) = runWriter $ unPredF predicate (defaultDist, r)  -- TODO: remember dist used by runTr?
    monitor (counterexample $ Text.unpack $ renderTraceContext testOutputs st)
    assert result

data GameModel = GameModel
    { gameValue     :: Integer
    , keeper        :: Maybe EM.Wallet
    , hasToken      :: Maybe EM.Wallet
    , currentSecret :: String
    , balances      :: Map EM.Wallet Integer
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

    type ActionMonad GameModel = Tr

    initialState = GameModel
        { gameValue     = 0
        , hasToken      = Nothing
        , keeper        = Nothing
        , currentSecret = ""
        , balances      = Map.empty
        , busy          = 0
        }

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
    precondition s Delay               = True

    nextState s (Lock w secret val)    _ = s { hasToken      = Just w
                                             , keeper        = Just w
                                             , currentSecret = secret
                                             , gameValue     = val
                                             , balances      = Map.singleton w (-val) }
    nextState s (Guess w old new val) _
        | busy s > 0                     = s
        | old /= currentSecret s         = busyFor 2 s
        | otherwise                      = busyFor 1 $ s
                                             { keeper        = Just w
                                             , currentSecret = new
                                             , gameValue     = gameValue s - val
                                             -- , balances      = Map.insert w val $ balances s    -- <== BUG
                                             , balances      = Map.insertWith (+) w val $ balances s
                                             }
    nextState s (PassToken _ w)        _
      | busy s > 0 = lessBusy s
      | otherwise  = lessBusy $ s { hasToken = Just w }
    nextState s Delay                  _ = lessBusy s

    postcondition s (Guess w _ _ _) _ (RetFail (HookError (EndpointNotActive (Just w') _)))
      | w==w' = busy s > 0
    postcondition _ _ _ RetOk     = True
    postcondition _ _ _ RetFail{} = False

    arbitraryAction s = oneof $
        [ Lock        <$> genWallet <*> genGuess <*> genValue | Nothing == hasToken s ] ++
        [ Guess w     <$> genGuess <*> genGuess <*> choose (1, gameValue s)
          | Just w <- [hasToken s], hasToken s /= keeper s, gameValue s > 0 ] ++
        [ PassToken w <$> genWallet | Just w <- [hasToken s] ] ++
        [ return Delay ]

    shrinkAction s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction s (PassToken w w') =
        [PassToken w w'' | w'' <- shrinkWallet w']
    shrinkAction s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]
    shrinkAction s Delay = []

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

    monitoring (_s,s) act _ res =
      case act of
        PassToken _ _ | busy _s > 0 -> classify True "passing-while-busy"
        _                           -> id
      . (counterexample $ show s)

lessBusy s = s { busy = 0 `max` (busy s - 1) }
busyFor n s = s { busy = n `max` busy s }

finalPredicate :: GameModel -> TracePredicate GameStateMachineSchema (TraceError G.GameError) ()
finalPredicate s = Map.foldrWithKey change top $ balances s
    where
        change w val rest = walletFundsChange w (Ada.lovelaceValueOf val <> gameTok) /\ rest
            where
                gameTok | Just w == hasToken s = gameTokenVal
                        | otherwise            = mempty

wallets = [w1, w2, w3]

genWallet :: Gen EM.Wallet
genWallet = elements wallets

shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getPositive <$> arbitrary

delay n = do
  replicateM n $ do
    mapM handleBlockchainEvents wallets
    addBlocks 1
  return ()

prop_Game :: Shrink2 (Script GameModel) -> Property
prop_Game (Shrink2 s) = monadic runTr $ do
    (st, _) <- runScript s
    run $ delay 10
    assertPredicate (finalPredicate st)

-- * Unit tests

tests :: TestTree
tests =
    testGroup "state machine tests"
    [ checkPredicate @GameStateMachineSchema "run a successful game trace"
        G.contract
        (walletFundsChange w2 (Ada.lovelaceValueOf 3 <> gameTokenVal)
        /\ fundsAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 5 ==)
        /\ walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        successTrace

    , checkPredicate @GameStateMachineSchema "run a 2nd successful game trace"
        G.contract
        (walletFundsChange w2 (Ada.lovelaceValueOf 3)
        /\ fundsAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 1 ==)
        /\ walletFundsChange w1 (Ada.lovelaceValueOf (-4) <> gameTokenVal))
        ( successTrace
        >> payToWallet w2 w1 gameTokenVal
        >> addBlocks 1
        >> handleBlockchainEvents w1
        >> callEndpoint @"guess" w1 GuessArgs{guessArgsOldSecret="new secret", guessArgsNewSecret="hello", guessArgsValueTakenOut=Ada.lovelaceValueOf 4}
        >> handleBlockchainEvents w1
        >> addBlocks 1
        )

    , checkPredicate @GameStateMachineSchema "run a failed trace"
        G.contract
        (walletFundsChange w2 gameTokenVal
        /\ fundsAtAddress (Scripts.scriptAddress G.scriptInstance) (Ada.lovelaceValueOf 8 ==)
        /\ walletFundsChange w1 (Ada.lovelaceValueOf (-8)))
        ( callEndpoint @"lock" w1 LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
        >> handleBlockchainEvents w1
        >> addBlocks 1
        >> handleBlockchainEvents w1
        >> addBlocks 1
        >> payToWallet w1 w2 gameTokenVal
        >> addBlocks 1
        >> callEndpoint @"guess" w2 GuessArgs{guessArgsOldSecret="hola", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
        >> handleBlockchainEvents w2
        >> addBlocks 1)


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

successTrace :: ContractTrace GameStateMachineSchema e a ()
successTrace = do
    callEndpoint @"lock" w1 LockArgs{lockArgsSecret="hello", lockArgsValue= Ada.lovelaceValueOf 8}
    handleBlockchainEvents w1
    addBlocks 1
    handleBlockchainEvents w1
    addBlocks 1
    payToWallet w1 w2 gameTokenVal
    addBlocks 1
    callEndpoint @"guess" w2 GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret="new secret", guessArgsValueTakenOut=Ada.lovelaceValueOf 3}
    handleBlockchainEvents w2
    addBlocks 1

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"

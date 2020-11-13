{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.GameStateMachine where

import           Test.Tasty
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Wallet.Emulator                                           as EM

-- * QuickCheck model

type Tr = ContractTrace GameStateMachineSchema G.GameError ()

runTr :: Tr Property -> Property
runTr tr =
  case runTraceWithDistribution defaultDist G.contract tr of
    (Right (p, _), _) -> p
    (Left err, _s)    -> whenFail (print err) False

data GameModel = GameModel
  { gameValue     :: Integer
  , keeper        :: Maybe EM.Wallet
  , hasToken      :: Maybe EM.Wallet
  , currentSecret :: String }
  deriving (Show)

instance StateModel GameModel where
  data Action GameModel = Lock EM.Wallet String Integer
                        | Guess EM.Wallet String String Integer
                        | PassToken EM.Wallet EM.Wallet
    deriving (Show)

  data Ret GameModel = RetOk
    deriving (Show)

  type ActionMonad GameModel = Tr

  initialState = GameModel
    { gameValue     = 0
    , hasToken      = Nothing
    , keeper        = Nothing
    , currentSecret = ""
    }

  precondition s (Lock _ _ _)        = Nothing == hasToken s
  precondition s (Guess w old _ val) = and [ Just w == hasToken s
                                           , val <= gameValue s
                                           , old == currentSecret s
                                           , Just w /= keeper s ]
  precondition s (PassToken w w')    = and [ Just w == hasToken s
                                           , w /= w'
                                           , gameValue s > 0 ] -- stops the test

  nextState s (Lock w secret val) _   = s { hasToken = Just w, keeper = Just w, currentSecret = secret, gameValue = val }
  nextState s (Guess w _old new val) _ = s { keeper = Just w, currentSecret = new, gameValue = gameValue s - val }
  nextState s (PassToken _ w) _       = s { hasToken = Just w }

  arbitraryAction s = oneof $
    [ Lock        <$> genWallet <*> genGuess <*> genValue | Nothing == hasToken s ] ++
    [ Guess w     <$> pure (currentSecret s) <*> genGuess <*> choose (1, gameValue s)
      | Just w <- [hasToken s], hasToken s /= keeper s, gameValue s > 0 ] ++
    [ PassToken w <$> genWallet | Just w <- [hasToken s] ]

  perform (Lock w new val) _ = RetOk <$ do
    callEndpoint @"lock" w LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
    handleBlockchainEvents w
    addBlocks 1
    handleBlockchainEvents w
    addBlocks 1
  perform (Guess w old new val) _ = RetOk <$ do
    callEndpoint @"guess" w GuessArgs{guessArgsOldSecret = old, guessArgsNewSecret = new, guessArgsValueTakenOut = Ada.lovelaceValueOf val}
    handleBlockchainEvents w
    addBlocks 1
  perform (PassToken w w') _ = RetOk <$ do
    payToWallet w w' gameTokenVal
    addBlocks 1
    handleBlockchainEvents w'

genWallet :: Gen EM.Wallet
genWallet = elements [w1, w2, w3]

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getPositive <$> arbitrary

prop_Game :: Script GameModel -> Property
prop_Game s = monadic runTr $ () <$ runScript s

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

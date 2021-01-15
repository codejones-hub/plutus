{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.TypedGameStateMachine where

import           Control.Monad
import           Control.Monad.Freer.Error
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Test.QuickCheck                                           hiding ((.&&.))
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.TypedStateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value)
import           Plutus.Trace.Emulator
import qualified Wallet.Emulator                                           as EM

-- * QuickCheck model


data GameModel = GameModel
    { gameValue     :: Integer
    , handles       :: Map EM.Wallet Handle
    , keeper        :: Maybe EM.Wallet
    , hasToken      :: Maybe EM.Wallet
    , currentSecret :: String
    , balances      :: Map EM.Wallet Integer
    , tokenLock     :: Integer
    , busy          :: Integer }
    deriving (Show)

newtype Handle = Handle { unHandle :: ContractHandle GameStateMachineSchema G.GameError }

instance Show Handle where
    show _ = "()"

deriving instance Show (Action GameModel a)

instance StateModel GameModel where
    data Action GameModel a where
      Lock      :: EM.Wallet -> String -> Integer           -> Action GameModel Ret
      Guess     :: EM.Wallet -> String -> String -> Integer -> Action GameModel Ret
      GiveToken :: EM.Wallet                                -> Action GameModel Ret
      Delay     ::                                             Action GameModel Ret

    type ActionMonad GameModel = EmulatorTrace

    initialState = GameModel
        { gameValue     = 0
        , handles       = Map.empty
        , hasToken      = Nothing
        , keeper        = Nothing
        , currentSecret = ""
        , balances      = Map.empty
        , tokenLock     = 0
        , busy          = 0
        }

    isFinal _ = False

    precondition s (Lock _ _ _)        = Nothing == hasToken s
    precondition s (Guess w _old _ val)= and [ Just w == hasToken s
                                             , val <= gameValue s
                                             -- , _old == currentSecret s
                                             -- , busy s == 0   -- <== precondition to avoid inactive endpoint
                                             , Just w /= keeper s ]
    precondition s (GiveToken w)       = and [ hasToken s `notElem` [Nothing, Just w]
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
    nextState s (GiveToken w) _
      | tokenLock s > 0 = lessBusy s
      | otherwise       = lessBusy $ s { hasToken = Just w }
    nextState s Delay _ = lessBusy s

    postcondition s (Guess w _ _ _) _ (RetFail (HookError (EndpointNotActive (Just w') _)))
      | w == w' = busy s > 0
    postcondition _ (Lock _ _ _)    _ ok = isOK ok
    postcondition _ (Guess _ _ _ _) _ ok = isOK ok
    postcondition _ (GiveToken _)   _ ok = isOK ok
    postcondition _ Delay           _ ok = isOK ok

    arbitraryAction s = oneof $
        [ Action <$> (Lock        <$> genWallet <*> genGuess <*> genValue) | Nothing == hasToken s ] ++
        [ Action <$> (Guess w     <$> genGuess <*> genGuess <*> choose (1, gameValue s))
          | Just w <- [hasToken s], hasToken s /= keeper s, gameValue s > 0 ] ++
        [ Action <$> (GiveToken   <$> genWallet) | hasToken s /= Nothing ] ++
        [ Action <$> return Delay ]

    shrinkAction _s (Lock w secret val) =
        [Action $ Lock w' secret val | w' <- shrinkWallet w] ++
        [Action $ Lock w secret val' | val' <- shrink val]
    shrinkAction _s (GiveToken w) =
        [Action $ GiveToken w' | w' <- shrinkWallet w]
    shrinkAction _s (Guess w old new val) =
        [Action $ Guess w' old new val | w' <- shrinkWallet w] ++
        [Action $ Guess w old new val' | val' <- shrink val]
    shrinkAction _s Delay = []

    perform s cmd _env = case cmd of
        Lock w new val -> handle $ do
            callEndpoint @"lock" (ctHandle w) LockArgs{lockArgsSecret = new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> handle $ do
            callEndpoint @"guess" (ctHandle w) GuessArgs{guessArgsOldSecret = old, guessArgsNewSecret = new, guessArgsValueTakenOut = Ada.lovelaceValueOf val}
        GiveToken w' -> handle $ do
            let Just w = hasToken s
            payToWallet w w' gameTokenVal
            delay 1
        Delay -> handle $ delay 1
        where
            handle m = RetOk <$ m -- catchError (RetOk <$ m) (return . RetFail)
            ctHandle w = unHandle (handles s Map.! w)

    monitoring (s0, s1) act _env _res =
      case act of
        GiveToken _ | busy s0 > 0 -> classify True "passing-while-busy"
        Guess _ guess _ _
          | currentSecret s0 == guess -> classify True "guessing-correctly"
          | currentSecret s0 /= guess -> classify True "guessing-wrongly"
        _                         -> id
      . (counterexample $ show s1)

lessBusy :: GameModel -> GameModel
lessBusy s = s { busy = 0 `max` (busy s - 1), tokenLock = 0 `max` (tokenLock s - 1) }

busyFor :: Integer -> Integer -> GameModel -> GameModel
busyFor n t s = s { busy = n `max` busy s, tokenLock = t `max` tokenLock s }

data Ret = RetOk | RetFail (TraceError G.GameError)
  deriving Show

isOK :: Ret -> Bool
isOK RetOk = True
isOK _     = False

finalPredicate :: GameModel -> TracePredicate
finalPredicate s = Map.foldrWithKey change (pure True) $ balances s
    where
        change w val rest = walletFundsChange w (Ada.lovelaceValueOf val <> gameTok) .&&. rest
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
delay n = void $ waitNSlots (fromIntegral n)

prop_Game :: Shrink2 (Script GameModel) -> Property
prop_Game (Shrink2 s) = propRunScript finalPredicate setup s $ \ _ -> run (delay 10)
    where
        setup = do
            hs <- run $ mapM (flip activateContractWallet G.contract) wallets
            return initialState{ handles = Map.fromList $ zip wallets $ map Handle hs }

-- Generic property to check that we don't get stuck. This only tests the model, but if the model
-- thinks it's not stuck, but the actual implementation is, the property running the contract will
-- fail. Except: this is only true if the precondition does not admit invalid transactions. At the
-- moment the off-chain contract breaks if you try to take an invalid transaction, so this isn't a
-- problem.
prop_notStuck :: StateModel state => Shrink2 (Script state) -> Property
prop_notStuck (Shrink2 s) = notStuck s

initialVal :: Value
initialVal = Ada.adaValueOf 10

w1 :: EM.Wallet
w1 = EM.Wallet 1

w2 :: EM.Wallet
w2 = EM.Wallet 2

w3 :: EM.Wallet
w3 = EM.Wallet 3

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.monetaryPolicyHash G.scriptInstance
    in G.token sym "guess"

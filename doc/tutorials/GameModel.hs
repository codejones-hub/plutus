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
import           Data.List
import           Data.Map                                                  (Map)
import qualified Data.Map                                                  as Map
import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Data.Void
-- START_IMPORT_QC
import           Test.QuickCheck                                           as QC hiding ((.&&.))
-- END_IMPORT_QC
import           Test.Tasty                                                hiding (after)
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

-- START_IMPORT_CONTRACT_TEST
import           Language.Plutus.Contract.Test
-- END_IMPORT_CONTRACT_TEST
-- START_IMPORT_CONTRACT_MODEL
import           Language.Plutus.Contract.Test.ContractModel
-- END_IMPORT_CONTRACT_MODEL
import           Language.Plutus.Contract.Test.StateModel                  (stateAfter)
import qualified Language.Plutus.Contract.Test.StateModel                  as StateModel
import           Language.PlutusTx.Coordination.Contracts.GameStateMachine as G
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import qualified Ledger.Typed.Scripts                                      as Scripts
import           Ledger.Value                                              (Value, isZero)
import           Plutus.Trace.Emulator                                     as Trace
import           Wallet.Emulator                                           (Wallet)
import           Wallet.Emulator.Folds                                     (postMapM)

-- * QuickCheck model

-- START_MODELSTATE
-- START_MODEL_TYPE
data GameModel = GameModel
-- END_MODEL_TYPE
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
    deriving (Show)
-- END_MODELSTATE

makeLenses 'GameModel

-- START_DEFINE_WALLETS
w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

wallets = [w1, w2, w3]
-- END_DEFINE_WALLETS

-- START_INSTANCE
instance ContractModel GameModel where

    data Action GameModel = Lock      Wallet String Integer
                          | Guess     Wallet String String Integer
                          | GiveToken Wallet
        deriving (Eq, Show)
-- END_INSTANCE

-- START_HANDLE_KEY
    data HandleKey GameModel schema err where
        WalletKey :: Wallet -> HandleKey GameModel GameStateMachineSchema GameError
-- END_HANDLE_KEY

-- START_ARBITRARY
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue              ] ++
        [ Guess     <$> genWallet <*> genGuess <*> genGuess <*> genValue ] ++
        [ GiveToken <$> genWallet                                        ]
-- END_ARBITRARY

-- START_GENERATORS
genWallet :: Gen Wallet
genWallet = elements wallets

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary
-- END_GENERATORS

-- START_GAME_PROPERTY
prop_Game :: Script GameModel -> Property
prop_Game script = propRunScript_ handleSpec script
-- END_GAME_PROPERTY

-- START_HANDLE_SPEC
handleSpec :: [HandleSpec GameModel]
handleSpec = [ HandleSpec (WalletKey w) w G.contract | w <- wallets ]
-- END_HANDLE_SPEC

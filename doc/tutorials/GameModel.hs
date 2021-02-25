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
import           Test.QuickCheck                                           as QC hiding ((.&&.))
import           Test.Tasty                                                hiding (after)
import qualified Test.Tasty.HUnit                                          as HUnit

import qualified Spec.Lib                                                  as Lib

import qualified Language.PlutusTx                                         as PlutusTx

import           Language.Plutus.Contract.Test                             hiding (not)
import           Language.Plutus.Contract.Test.ContractModel
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
data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
    deriving (Show)
-- END_MODELSTATE

makeLenses 'GameModel


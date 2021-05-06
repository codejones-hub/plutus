{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
-- | A state machine with two states and two roles that take turns.
module Plutus.Contracts.PingPong(
    PingPongState(..),
    Input(..),
    PingPongError(..),
    PingPongSchema,
    runPing,
    runPong,
    initialise,
    runStop,
    runWaitForUpdate,
    combined
    ) where

import           Control.Lens
import           Control.Monad                (forever, void)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           GHC.Generics                 (Generic)
import qualified Ledger.Ada                   as Ada
import           Ledger.Constraints           (TxConstraints)
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx              (TypedScriptTxOut (..))
import qualified PlutusTx                     as PlutusTx
import           PlutusTx.Prelude             hiding (Applicative (..), check)

import           Plutus.Contract
import           Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified Prelude                      as Haskell

data PingPongState = Pinged | Ponged | Stopped
    deriving stock (Haskell.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq PingPongState where
    Pinged == Pinged = True
    Ponged == Ponged = True
    _ == _           = False

data Input = Ping | Pong | Stop
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type PingPongSchema =
    BlockchainActions
        .\/ Endpoint "initialise" ()
        .\/ Endpoint "ping" ()
        .\/ Endpoint "pong" ()
        .\/ Endpoint "stop" () -- Transition the state machine instance to the final state
        .\/ Endpoint "wait" () -- Wait for a change to the on-chain state of the machine

data PingPongError =
    PingPongContractError ContractError
    | PingPongSMError SM.SMContractError
    | StoppedUnexpectedly
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PingPongError

instance AsSMContractError PingPongError where
    _SMContractError = _PingPongSMError

instance AsContractError PingPongError where
    _ContractError = _PingPongContractError

{-# INLINABLE transition #-}
transition :: State PingPongState -> Input -> Maybe (TxConstraints Void Void, State PingPongState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (_,      Stop) -> Just (mempty, State{stateData=Stopped, stateValue=mempty})
    (Pinged, Pong) -> Just (mempty, State{stateData=Ponged, stateValue})
    (Ponged, Ping) -> Just (mempty, State{stateData=Pinged, stateValue})
    _              -> Nothing

{-# INLINABLE machine #-}
machine :: SM.StateMachine PingPongState Input
machine = SM.mkStateMachine transition isFinal where
    isFinal Stopped = True
    isFinal _       = False

{-# INLINABLE mkValidator #-}
mkValidator :: Scripts.ValidatorType (SM.StateMachine PingPongState Input)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.ScriptInstance (SM.StateMachine PingPongState Input)
scriptInstance = Scripts.validator @(SM.StateMachine PingPongState Input)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

machineInstance :: SM.StateMachineInstance PingPongState Input
machineInstance = SM.StateMachineInstance machine scriptInstance

type PingPongClient = SM.StateMachineClient PingPongState Input

initialise :: Contract SM.SMOutput PingPongSchema PingPongError PingPongClient
initialise = endpoint @"initialise" >> SM.runInitialise machineInstance Pinged (Ada.lovelaceValueOf 1)

run ::
    forall w.
    PingPongClient
    -> PingPongState
    -> Contract w PingPongSchema PingPongError ()
    -> Contract w PingPongSchema PingPongError ()
run client expectedState action = do
    let extractState = fst . tyTxOutData . fst
        go Nothing = throwError StoppedUnexpectedly
        go (Just currentState)
            | extractState currentState == expectedState = action
            | otherwise = SM.waitForUpdate client >>= go
    maybeState <- SM.getOnChainState client
    let datum = fmap fst maybeState
    go datum

runPing :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError ()
runPing client = run client Ponged (ping client)

ping :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError ()
ping client = endpoint @"ping" >> void (SM.runStep client Ping)

runPong :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError ()
runPong client = run client Pinged (pong client)

pong :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError ()
pong client = endpoint @"pong" >> void (SM.runStep client Pong)

runStop :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError ()
runStop client = endpoint @"stop" >> void (SM.runStep client Stop)

runWaitForUpdate :: forall w. PingPongClient -> Contract w PingPongSchema PingPongError (Maybe (OnChainState PingPongState Input))
runWaitForUpdate = SM.waitForUpdate

combined :: Contract SM.SMOutput PingPongSchema PingPongError ()
combined = do
    client <- initialise
    forever (ping client `select` pong client `select` runStop client `select` wait client)
    where
        wait client = do
            _ <- endpoint @"wait"
            newState <- runWaitForUpdate client
            case newState of
                Nothing -> logWarn @String "runWaitForUpdate: Nothing"
                Just (TypedScriptTxOut{tyTxOutData=(s, _)}, _) -> do
                    logInfo $ "new state: " <> show s
                    -- tell (Last $ Just s)

PlutusTx.unstableMakeIsData ''PingPongState
PlutusTx.makeLift ''PingPongState
PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input

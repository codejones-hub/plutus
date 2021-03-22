{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Main(main) where

import           Control.Monad                            (void)
import           Control.Monad.Freer                      (Eff, Member, reinterpret, type (~>))
import           Control.Monad.Freer.Error                (Error)
import           Control.Monad.Freer.Extras.Log           (LogMsg)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Bifunctor                           (Bifunctor (first))
import           Data.Row
import           Data.Text.Extras                         (tshow)
import           Data.Text.Prettyprint.Doc                (Pretty (..), viaShow)
import           GHC.Generics                             (Generic)
import qualified Language.Marlowe.Client                  as Marlowe
import           Playground.Schema                        (endpointsToSchemas)
import           Plutus.PAB.Effects.Contract              (ContractEffect (..), PABContract (..))
import           Plutus.PAB.Effects.Contract.ContractTest (doContractInit, doContractUpdate)
import           Plutus.PAB.Events.Contract               (ContractPABRequest)
import           Plutus.PAB.Events.ContractInstanceState  (PartiallyDecodedResponse)
import           Plutus.PAB.Monitoring.PABLogMsg          (ContractEffectMsg (..))
import           Plutus.PAB.Simulator                     (SimulatorContractHandler, SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                     as Simulator
import           Plutus.PAB.Types                         (PABError (..))
import qualified Plutus.PAB.Webserver.Server              as PAB.Server

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    liftIO $ putStrLn "Starting marlowe PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    -- You can add simulator actions here:
    -- Simulator.activateContract
    -- Simulator.callEndpointOnInstance
    -- Simulator.observableState
    -- etc.
    -- That way, the simulation gets to a predefined state and you don't have to
    -- use the HTTP API for setup.
    _ <- liftIO getLine
    shutdown

data Marlowe

data MarloweContracts =
    MarloweApp -- the main marlowe contract
    | WalletCompanion -- TODO: actually implement this!
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance PABContract Marlowe where
    type ContractDef Marlowe = MarloweContracts
    type State Marlowe = PartiallyDecodedResponse ContractPABRequest
    serialisableState _ = id

instance Pretty MarloweContracts where
    pretty = viaShow

handleMarloweContract ::
    ( Member (Error PABError) effs
    , Member (LogMsg ContractEffectMsg) effs
    )
    => ContractEffect Marlowe
    ~> Eff effs
handleMarloweContract = \case
    InitialState c -> case c of
        MarloweApp      -> doContractInit marlowe
        WalletCompanion -> error "wallet companion not implemented"
    UpdateContract c state p -> case c of
        MarloweApp      -> doContractUpdate marlowe state p
        WalletCompanion -> error "wallet companion not implemented"
    ExportSchema t -> case t of
        MarloweApp      -> pure $ endpointsToSchemas @Empty
        WalletCompanion -> pure $ endpointsToSchemas @Empty
            -- TODO:
            -- replace with (Marlowe.MarloweSchema .\\ BlockchainActions)
            -- (needs some instances for the Marlowe types (MarloweParams, etc))
    where
        marlowe = first tshow Marlowe.marlowePlutusContract

handlers :: SimulatorEffectHandlers Marlowe
handlers = Simulator.mkSimulatorHandlers @Marlowe [MarloweApp] mlw where
    mlw :: SimulatorContractHandler Marlowe
    mlw =
        Simulator.handleContractEffectMsg @Marlowe
        . reinterpret handleMarloweContract

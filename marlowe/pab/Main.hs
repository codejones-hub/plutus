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

import           Control.Monad.Freer                      (Eff, Member, type (~>))
import           Control.Monad.Freer.Error                (Error)
import           Control.Monad.Freer.Extras.Log           (LogMsg)
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
import qualified Plutus.PAB.Simulator                     as Simulator
import           Plutus.PAB.Types                         (PABError (..))

main :: IO ()
main = do
    pure ()

data Marlowe

data MarloweContracts = Marlowe -- TODO: Add a constructor for the companion contract
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
        Marlowe -> doContractInit marlowe
    UpdateContract c state p -> case c of
        Marlowe -> doContractUpdate marlowe state p
    ExportSchema t -> case t of
        Marlowe -> pure $ endpointsToSchemas @Empty
            -- TODO:
            -- replace with (Marlowe.MarloweSchema .\\ BlockchainActions)
            -- (needs some instances for the Marlowe types (MarloweParams, etc))
    where
        marlowe = first tshow Marlowe.marlowePlutusContract

marloweSimulatorHandlers :: EffectHandlers Marlowe (SimulatorState Marlowe)
marloweSimulatorHandlers = undefined

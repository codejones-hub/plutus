{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Plutus.Contract.Effects.WriteTx where

import           Control.Lens
import           Data.Aeson                               (FromJSON, ToJSON)
import           Data.Row
import           Data.Text.Prettyprint.Doc
import           Data.Void                                (Void)
import           GHC.Generics                             (Generic)

import           Plutus.Contract.Effects.AwaitTxConfirmed (HasTxConfirmation, awaitTxConfirmed)
import           Plutus.Contract.Request                  as Req
import           Plutus.Contract.Schema                   (Event (..), Handlers (..), Input, Output)
import           Plutus.Contract.Types                    (AsContractError, Contract, _ConstraintResolutionError,
                                                           _WalletError, throwError)
import qualified PlutusTx                                 as PlutusTx

import           Ledger.AddressMap                        (UtxoMap)
import           Ledger.Constraints                       (TxConstraints)
import           Ledger.Constraints.OffChain              (ScriptLookups, UnbalancedTx)
import qualified Ledger.Constraints.OffChain              as Constraints
import           Ledger.Tx                                (Tx, txId)
import           Ledger.Typed.Scripts                     (ScriptInstance, ScriptType (..))

import           Wallet.API                               (WalletAPIError)

type UnbalancedTxSymbol = "utx"
type BalancedTxSymbol = "btx"

data WriteTxResponse =
  WriteTxFailed WalletAPIError
  | WriteTxSuccess Tx
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty WriteTxResponse where
  pretty = \case
    WriteTxFailed e  -> "WriteTxFailed:" <+> pretty e
    WriteTxSuccess i -> "WriteTxSuccess:" <+> pretty (txId i)

writeTxResponse :: Iso' WriteTxResponse (Either WalletAPIError Tx)
writeTxResponse = iso f g where
  f = \case { WriteTxFailed w -> Left w; WriteTxSuccess t -> Right t }
  g = either WriteTxFailed WriteTxSuccess

type HasWriteUnbalancedTx s =
    ( HasType UnbalancedTxSymbol WriteTxResponse (Input s)
    , HasType UnbalancedTxSymbol UnbalancedTx (Output s)
    , ContractRow s)

type WriteUnbalancedTx = UnbalancedTxSymbol .== (WriteTxResponse, UnbalancedTx)

type HasWriteBalancedTx s =
    ( HasType BalancedTxSymbol WriteTxResponse (Input s)
    , HasType BalancedTxSymbol Tx (Output s)
    , ContractRow s)

type WriteBalancedTx = BalancedTxSymbol .== (WriteTxResponse, Tx)

type HasWriteTx s = (HasWriteUnbalancedTx s, HasWriteBalancedTx s)

type WriteTx = WriteUnbalancedTx .\/ WriteBalancedTx

-- | Send an unbalanced transaction to be balanced and signed. Returns the ID
--    of the final transaction when the transaction was submitted. Throws an
--    error if balancing or signing failed.
submitUnbalancedTx :: forall w s e. (AsContractError e, HasWriteTx s) => UnbalancedTx -> Contract w s e Tx
submitUnbalancedTx utx = do
  tx <- balanceTx utx
  submitBalancedTx tx

balanceTx :: forall w s e. (AsContractError e, HasWriteUnbalancedTx s) => UnbalancedTx -> Contract w s e Tx
-- See Note [Injecting errors into the user's error type]
balanceTx t =
  let req = request @w @UnbalancedTxSymbol @_ @_ @s t in
  req >>= either (throwError . review _WalletError) pure . view writeTxResponse

submitBalancedTx :: forall w s e. (AsContractError e, HasWriteTx s) => Tx -> Contract w s e Tx
-- See Note [Injecting errors into the user's error type]
submitBalancedTx t =
  let req = request @w @BalancedTxSymbol @_ @_ @s t in
  req >>= either (throwError . review _WalletError) pure . view writeTxResponse

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. The constraints do not refer to any typed script inputs or
--   outputs.
submitTx :: forall w s e.
  ( HasWriteTx s
  , AsContractError e
  )
  => TxConstraints Void Void
  -> Contract w s e Tx
submitTx = submitTxConstraintsWith @Void mempty

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the current outputs at the contract address and the
--   contract's own public key to solve the constraints.
submitTxConstraints
  :: forall a w s e.
  ( HasWriteTx s
  , PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => ScriptInstance a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraints inst = submitTxConstraintsWith (Constraints.scriptInstanceLookups inst)

-- | Build a transaction that satisfies the constraints using the UTXO map
--   to resolve any input constraints (see 'Ledger.Constraints.TxConstraints.InputConstraint')
submitTxConstraintsSpending
  :: forall a w s e.
  ( HasWriteTx s
  , PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => ScriptInstance a
  -> UtxoMap
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraintsSpending inst utxo =
  let lookups = Constraints.scriptInstanceLookups inst <> Constraints.unspentOutputs utxo
  in submitTxConstraintsWith lookups

-- | Build a transaction that satisfies the constraints, then submit it to the
--   network. Using the given constraints.
submitTxConstraintsWith
  :: forall a w s e.
  ( HasWriteTx s
  , PlutusTx.IsData (RedeemerType a)
  , PlutusTx.IsData (DatumType a)
  , AsContractError e
  )
  => ScriptLookups a
  -> TxConstraints (RedeemerType a) (DatumType a)
  -> Contract w s e Tx
submitTxConstraintsWith sl constraints = do
  tx <- either (throwError . review _ConstraintResolutionError) pure (Constraints.mkTx sl constraints)
  submitUnbalancedTx tx

-- | A version of 'submitTx' that waits until the transaction has been
--   confirmed on the ledger before returning.
submitTxConfirmed
  :: forall w s e.
  ( HasWriteTx s
  , HasTxConfirmation s
  , AsContractError e
  )
  => UnbalancedTx
  -> Contract w s e ()
submitTxConfirmed t = submitUnbalancedTx t >>= awaitTxConfirmed . txId

unbalancedEvent
  :: forall s. (HasType UnbalancedTxSymbol WriteTxResponse (Input s), AllUniqueLabels (Input s))
  => WriteTxResponse
  -> Event s
unbalancedEvent r = Event (IsJust #utx r)

balancedEvent
  :: forall s. (HasType BalancedTxSymbol WriteTxResponse (Input s), AllUniqueLabels (Input s))
  => WriteTxResponse
  -> Event s
balancedEvent r = Event (IsJust #btx r)

unbalancedTransaction
  :: forall s. ( HasType UnbalancedTxSymbol UnbalancedTx (Output s) )
   => Handlers s
   -> Maybe UnbalancedTx
unbalancedTransaction (Handlers r) = trial' r (Label @UnbalancedTxSymbol)

pendingTransaction
  :: forall s. ( HasType BalancedTxSymbol Tx (Output s) )
   => Handlers s
   -> Maybe Tx
pendingTransaction (Handlers r) = trial' r (Label @BalancedTxSymbol)

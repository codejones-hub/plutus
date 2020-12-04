{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Spec.Prism (tests, prop_Prism) where

import           Control.Monad
import           Control.Monad.Freer.Error
import           Data.Foldable                                             (traverse_)
import           Language.Plutus.Contract.Test
import           Language.Plutus.Contract.Test.StateModel
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import           Ledger.Crypto                                             (pubKeyHash)
import           Ledger.Value                                              (TokenName)
import           Wallet.Emulator.Notify                                    (walletInstanceId)

import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty

import           Language.PlutusTx.Coordination.Contracts.Prism            hiding (credentialManager, mirror)
import qualified Language.PlutusTx.Coordination.Contracts.Prism.Credential as Credential
import           Language.PlutusTx.Coordination.Contracts.Prism.STO        (STOData (..))
import qualified Language.PlutusTx.Coordination.Contracts.Prism.STO        as STO
import           Language.PlutusTx.Monoid

user, credentialManager, mirror, issuer :: Wallet
user = Wallet 1
mirror = Wallet 2
credentialManager = Wallet 3
issuer = Wallet 4

kyc :: TokenName
kyc = "KYC"

sto :: TokenName
sto = "STO token"

numTokens :: Integer
numTokens = 1000

credential :: Credential
credential =
    Credential
        { credName = kyc
        , credAuthority = CredentialAuthority (pubKeyHash $ walletPubKey mirror)
        }

stoSubscriber :: STOSubscriber
stoSubscriber =
    STOSubscriber
        { wCredential = credential
        , wSTOIssuer = pubKeyHash $ walletPubKey issuer
        , wSTOTokenName = sto
        , wSTOAmount = numTokens
        }

stoData :: STOData
stoData =
    STOData
        { stoIssuer = pubKeyHash $ walletPubKey issuer
        , stoTokenName = sto
        , stoCredentialToken = Credential.token credential
        }

tests :: TestTree
tests = testGroup "PRISM"
    [ checkPredicate "withdraw"
        contract
        (assertDone user (const True) ""
        /\ walletFundsChange issuer (Ada.lovelaceValueOf numTokens)
        /\ walletFundsChange user (Ada.lovelaceValueOf (negate numTokens) <> STO.coins stoData numTokens)
        )
        (callEndpoint @"role" user UnlockSTO
        >> callEndpoint @"role" mirror Mirror
        >> callEndpoint @"role" credentialManager CredMan
        >> handleAll

        -- issue a KYC credential to a user
        >> callEndpoint @"issue" mirror CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        >> handleBlockchainEvents mirror
        >> addBlocks 1
        >> handleAll
        >> addBlocks 1
        >> handleAll

        -- participate in STO presenting the token
        >> callEndpoint @"sto" user stoSubscriber
        >> handleBlockchainEvents user
        >> callEndpoint @"credential manager" user (walletInstanceId credentialManager)
        >> handleBlockchainEvents user
        >> handleBlockchainEvents credentialManager
        >> handleBlockchainEvents user
        >> addBlocks 1
        >> handleAll
        >> addBlocks 1
        >> handleAll

        )
    ]
    where
        handleAll = traverse_ handleBlockchainEvents [user, mirror, credentialManager, issuer]

-- * QuickCheck model

data PrismModel = PrismModel
    { isIssued    :: Bool
    , pendingCall :: Bool
    , balance     :: Integer }

instance StateModel PrismModel where

    data Action PrismModel = Delay | Issue | Revoke | Call | Present
        deriving (Show)

    data Ret PrismModel = RetOk | RetFail (TraceError PrismError)
        deriving (Show)

    type ActionMonad PrismModel = ContractTrace PrismSchema PrismError ()

    arbitraryAction _ = oneof $
        [ pure Delay
        , pure Revoke
        , pure Issue
        , pure Call
        , pure Present ]

    initialState = PrismModel { isIssued = False, pendingCall = False, balance = 0 }

    nextState s Revoke  _           = s { isIssued = False }
    nextState s Issue   _           = s { isIssued = True }
    nextState s Call    _           = s { pendingCall = True }
    nextState s Present _
        | pendingCall s, isIssued s = s { pendingCall = False, balance = balance s + 1 }
    nextState s _       _           = s

                                 -- v Wait a generous amount of blocks between calls
    perform cmd _env = handle $ (>> delay 5) $ case cmd of
        Delay   -> return ()
        Issue   -> callEndpoint @"issue" mirror CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Revoke  -> callEndpoint @"revoke" mirror CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Call    -> callEndpoint @"sto" user stoSubscriber
        Present -> callEndpoint @"credential manager" user (walletInstanceId credentialManager)
        where
            handle m = catchError (RetOk <$ m) (return . RetFail)

    shrinkAction _ Delay = []
    shrinkAction _ _     = [Delay]

delay :: Int -> ActionMonad PrismModel ()
delay n = do
  replicateM_ n $ do
    mapM_ handleBlockchainEvents [user, mirror, credentialManager, issuer]
    addBlocks 1
  return ()

finalPredicate :: PrismModel -> TracePredicate PrismSchema (TraceError PrismError) ()
finalPredicate s = walletFundsChange issuer ada /\
                   walletFundsChange user (inv ada <> coin)
    where
        n    = numTokens * balance s
        ada  = Ada.lovelaceValueOf n
        coin = STO.coins stoData n

prop_Prism :: Script PrismModel -> Property
prop_Prism script = propRunScript finalPredicate contract before script after
    where
        before = run $ do
            callEndpoint @"role" user UnlockSTO
            callEndpoint @"role" mirror Mirror
            callEndpoint @"role" credentialManager CredMan
        after _ = return ()


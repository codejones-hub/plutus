{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Spec.Prism where -- (tests, prismTrace, prop_Prism) where

import           Control.Arrow                                             (first, second)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Data.Foldable                                             (traverse_)
import           Data.Maybe
import           Language.Plutus.Contract                                  (HasEndpoint)
import           Language.Plutus.Contract.Test                             hiding (not)
import           Language.Plutus.Contract.Test.ContractModel               as ContractModel
import           Language.PlutusTx.Lattice
import qualified Ledger.Ada                                                as Ada
import           Ledger.Crypto                                             (pubKeyHash)
import           Ledger.Value                                              (TokenName)

import           Test.QuickCheck                                           hiding ((.&&.))
import           Test.QuickCheck.Monadic
import           Test.Tasty

import           Language.PlutusTx.Coordination.Contracts.Prism            hiding (credentialManager, mirror)
import qualified Language.PlutusTx.Coordination.Contracts.Prism.Credential as Credential
import           Language.PlutusTx.Coordination.Contracts.Prism.STO        (STOData (..))
import qualified Language.PlutusTx.Coordination.Contracts.Prism.STO        as STO
import           Language.PlutusTx.Monoid                                  (inv)
import qualified Plutus.Trace.Emulator                                     as Trace

import           Debug.Trace

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
        (assertDone contract (Trace.walletInstanceTag user) (const True) ""
        .&&. walletFundsChange issuer (Ada.lovelaceValueOf numTokens)
        .&&. walletFundsChange user (Ada.lovelaceValueOf (negate numTokens) <> STO.coins stoData numTokens)
        )
        prismTrace
    ]

-- | 'mirror' issues a KYC token to 'user', who then uses it in an STO transaction
prismTrace :: Trace.EmulatorTrace ()
prismTrace = do
    uhandle <- Trace.activateContractWallet user contract
    mhandle <- Trace.activateContractWallet mirror contract
    chandle <- Trace.activateContractWallet credentialManager contract

    Trace.callEndpoint @"role" uhandle UnlockSTO
    Trace.callEndpoint @"role" mhandle Mirror
    Trace.callEndpoint @"role" chandle CredMan
    _ <- Trace.waitNSlots 2

    -- issue a KYC credential to a user
    Trace.callEndpoint @"issue" mhandle CredentialOwnerReference{coTokenName=kyc, coOwner=user}
    _ <- Trace.waitNSlots 2

    -- participate in STO presenting the token
    Trace.callEndpoint @"sto" uhandle stoSubscriber
    _ <- Trace.waitNSlots 2 -- needed?
    Trace.callEndpoint @"credential manager" uhandle (Trace.chInstanceId chandle)
    void $ Trace.waitNSlots 2

-- * QuickCheck model

data STOState = STOReady | STOPending | STODone
    deriving (Eq, Ord, Show)

data IssueState = NoIssue | Revoked | Issued
    deriving (Eq, Ord, Show)

data PrismModel = PrismModel
    { _isIssued :: IssueState
    , _stoState :: STOState
    }
    deriving (Show)

makeLenses 'PrismModel

doRevoke :: IssueState -> IssueState
doRevoke NoIssue = NoIssue
doRevoke Revoked = Revoked
doRevoke Issued  = Revoked

waitSlots :: Int
waitSlots = 2

deriving instance Show (Command PrismModel a)
deriving instance Eq   (Command PrismModel a)

instance ContractModel PrismModel where

    data Command PrismModel a where
        Delay   :: Command PrismModel ()
        Issue   :: Command PrismModel ()
        Revoke  :: Command PrismModel ()
        Call    :: Command PrismModel ()
        Present :: Command PrismModel ()

    type Schema PrismModel = PrismSchema
    type Err    PrismModel = PrismError

    arbitraryCommand _ = oneof $ map (Some <$>)
        [ pure Delay
        , pure Revoke
        , pure Issue
        , pure Call
        , pure Present ]

    initialState = PrismModel { _isIssued = NoIssue, _stoState = STOReady }

    precondition s Issue = (s ^. modelState . isIssued) /= Issued  -- Multiple Issue (without Revoke) breaks the contract
    precondition _ _     = True

    nextState cmd v = do
        wait waitSlots
        case cmd of
            Delay   -> wait 1
            Revoke  -> isIssued $~ doRevoke
            Issue   -> isIssued $= Issued
            Call    -> stoState $~ \ case STOReady -> STOPending; sto -> sto
            Present -> do
                iss <- (== Issued)     <$> getModelState isIssued
                sto <- (== STOPending) <$> getModelState stoState
                stoState $= STOReady
                when (iss && sto) $ do
                    transfer user issuer (Ada.lovelaceValueOf numTokens)
                    deposit user $ STO.coins stoData numTokens
                return ()

    perform s cmd _env = case cmd of
        Delay   -> wrap $ delay 1
        Issue   -> wrap $ Trace.callEndpoint @"issue"              (handle s mirror) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Revoke  -> wrap $ Trace.callEndpoint @"revoke"             (handle s mirror) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Call    -> wrap $ Trace.callEndpoint @"sto"                (handle s user) stoSubscriber
        Present -> wrap $ Trace.callEndpoint @"credential manager" (handle s user) (contractInstanceId s credentialManager)
        where                     -- v Wait a generous amount of blocks between calls
            wrap m   = m *> delay waitSlots

    shrinkCommand _ Delay = []
    shrinkCommand _ _     = [Some Delay]

    monitoring (_, s) _ _ _ = counterexample (show s)

delay :: Int -> Trace.EmulatorTrace ()
delay n = void $ Trace.waitNSlots $ fromIntegral n

finalPredicate :: ModelState PrismModel -> TracePredicate
finalPredicate _ =
    foldr (.&&.) (pure True)
        [ assertNotDone contract (Trace.walletInstanceTag w) "Contract stopped"
        | w <- [ issuer, user, mirror, credentialManager ] ]

prop_Prism :: Script PrismModel -> Property
prop_Prism script = propRunScript @PrismModel wallets contract finalPredicate before script after
    where
        wallets = [user, mirror, credentialManager] -- , issuer]
        before :: ModelState PrismModel -> Trace.EmulatorTrace ()
        before s = do
            Trace.callEndpoint @"role" (handle s user)              UnlockSTO
            Trace.callEndpoint @"role" (handle s mirror)            Mirror
            Trace.callEndpoint @"role" (handle s credentialManager) CredMan
            delay 5
        after _ = return () -- monitor $ collect $ s ^. balances . at user


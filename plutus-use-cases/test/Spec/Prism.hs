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

import           Test.QuickCheck                                           as QC hiding ((.&&.))
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

waitSlots :: Integer
waitSlots = 2

deriving instance Eq   (HandleKey PrismModel s e)
deriving instance Show (HandleKey PrismModel s e)

instance ContractModel PrismModel where

    data Action PrismModel = Setup | Delay | Issue | Revoke | Call | Present
        deriving (Eq, Show)

    data HandleKey PrismModel s e where
        MirrorH  :: HandleKey PrismModel PrismSchema PrismError
        UserH    :: HandleKey PrismModel PrismSchema PrismError
        ManagerH :: HandleKey PrismModel PrismSchema PrismError

    arbitraryAction _ = QC.elements [Delay, Revoke, Issue, Call, Present]

    initialState = PrismModel { _isIssued = NoIssue, _stoState = STOReady }

    precondition s Issue = (s ^. contractState . isIssued) /= Issued  -- Multiple Issue (without Revoke) breaks the contract
    precondition _ _     = True

    nextState cmd = do
        wait waitSlots
        case cmd of
            Setup   -> return ()
            Delay   -> wait 1
            Revoke  -> isIssued $~ doRevoke
            Issue   -> isIssued $= Issued
            Call    -> stoState $~ \ case STOReady -> STOPending; sto -> sto
            Present -> do
                iss <- (== Issued)     <$> viewContractState isIssued
                sto <- (== STOPending) <$> viewContractState stoState
                stoState $= STOReady
                when (iss && sto) $ do
                    transfer user issuer (Ada.lovelaceValueOf numTokens)
                    deposit user $ STO.coins stoData numTokens
                return ()

    perform handle s cmd = case cmd of
        Setup   -> do
            Trace.callEndpoint @"role" (handle UserH)    UnlockSTO
            Trace.callEndpoint @"role" (handle MirrorH)  Mirror
            Trace.callEndpoint @"role" (handle ManagerH) CredMan
            delay 5
        Delay   -> wrap $ delay 1
        Issue   -> wrap $ Trace.callEndpoint @"issue"              (handle MirrorH) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Revoke  -> wrap $ Trace.callEndpoint @"revoke"             (handle MirrorH) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Call    -> wrap $ Trace.callEndpoint @"sto"                (handle UserH) stoSubscriber
        Present -> wrap $ Trace.callEndpoint @"credential manager" (handle UserH) (Trace.chInstanceId $ handle ManagerH)
        where                     -- v Wait a generous amount of blocks between calls
            wrap m   = m *> delay waitSlots

    shrinkAction _ Delay = []
    shrinkAction _ _     = [Delay]

    monitoring (_, s) _ = counterexample (show s)

delay :: Integer -> Trace.EmulatorTrace ()
delay n = void $ Trace.waitNSlots $ fromIntegral n

finalPredicate :: ModelState PrismModel -> TracePredicate
finalPredicate _ =
    foldr (.&&.) (pure True)
        [ assertNotDone contract (Trace.walletInstanceTag w) "Contract stopped"
        | w <- [ issuer, user, mirror, credentialManager ] ]

prop_Prism :: Property     -- vvvvv Setup roles first
prop_Prism = forAllDL (action Setup >> anyActions_) $ propRunScript @PrismModel spec finalPredicate
    where
        spec = [ HandleSpec UserH    user contract
               , HandleSpec MirrorH  mirror contract
               , HandleSpec ManagerH credentialManager contract ]


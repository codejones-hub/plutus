{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Spec.Prism (tests, prismTrace, prop_Prism) where

import           Control.Arrow                                             (first, second)
import           Control.Monad
import           Control.Monad.Freer.Error
import           Data.Foldable                                             (traverse_)
import           Language.Plutus.Contract.Test                             hiding (not)
import           Language.Plutus.Contract.Test.StateModel
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

data PrismModel = PrismModel
    { isIssued      :: IssueState
    , stoState      :: STOState
    , balance       :: Integer
    , userHandle    :: Handle
    , mirrorHandle  :: Handle
    , managerHandle :: Handle
    , slot          :: Int
    }
    deriving (Show)

newtype Handle = Handle { unHandle :: Trace.ContractHandle PrismSchema PrismError }

instance Show Handle where
    show _ = "()"

data STOState = STOReady | STOPending | STODone
    deriving (Eq, Ord, Show)

data IssueState = NoIssue | Revoked | Issued | Broken
    deriving (Eq, Ord, Show)

doIssue :: IssueState -> IssueState
doIssue Broken  = Broken
doIssue NoIssue = Issued
doIssue Revoked = Issued
doIssue Issued  = Broken

doRevoke :: IssueState -> IssueState
doRevoke Broken  = Broken
doRevoke NoIssue = NoIssue
doRevoke Revoked = Revoked
doRevoke Issued  = Revoked

canSTO :: PrismModel -> Bool
canSTO s = isIssued s == Issued

waitSlots :: Int
waitSlots = 2

maxSlots :: Int
maxSlots = 125 - waitSlots - 5 - 2      -- default emulator runs for 125

tick :: PrismModel -> PrismModel
tick s = s { slot = slot s + waitSlots }

data Ret = RetOk | RetFail (TraceError PrismError)
    deriving (Show)

deriving instance Show (Action PrismModel a)
deriving instance Eq   (Action PrismModel a)

instance StateModel PrismModel where

    data Action PrismModel a where
        Delay   :: Action PrismModel Ret
        Issue   :: Action PrismModel Ret
        Revoke  :: Action PrismModel Ret
        Call    :: Action PrismModel Ret
        Present :: Action PrismModel Ret

    type ActionMonad PrismModel = Trace.EmulatorTrace

    arbitraryAction _ = oneof $ map (Action <$>)
        [ pure Delay
        , pure Revoke
        , pure Issue
        , pure Call
        , pure Present ]

    initialState = PrismModel { isIssued = NoIssue, stoState = STOReady, balance = 0
                              , userHandle    = error "filled in later"
                              , mirrorHandle  = error "filled in later"
                              , managerHandle = error "filled in later"
                              , slot = 0
                              }

    precondition s cmd   = (isIssued s /= Issued || isIssue cmd) && -- Multiple Issue (without Revoke) breaks the contract
                           slot s < maxSlots
        where isIssue Issue = True
              isIssue _     = False

    nextState s Revoke  _          = tick s{ isIssued = doRevoke $ isIssued s }
    nextState s Issue   _          = tick s{ isIssued = doIssue  $ isIssued s }
    nextState s Call    _
        | stoState s == STOReady   = tick s{ stoState = STOPending }
    nextState s Present _
        | stoState s == STOPending = tick s{ stoState = STOReady, balance = balance s + if canSTO s then 1 else 0 }
    nextState s _       _          = tick s

    perform PrismModel{mirrorHandle, userHandle, managerHandle} cmd _env = case cmd of
        Delay   -> wrap $ return ()
        Issue   -> wrap $ Trace.callEndpoint @"issue"              (unHandle mirrorHandle) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Revoke  -> wrap $ Trace.callEndpoint @"revoke"             (unHandle mirrorHandle) CredentialOwnerReference{coTokenName=kyc, coOwner=user}
        Call    -> wrap $ Trace.callEndpoint @"sto"                (unHandle userHandle) stoSubscriber
        Present -> wrap $ Trace.callEndpoint @"credential manager" (unHandle userHandle) (Trace.chInstanceId $ unHandle managerHandle)
        where
                                  -- v Wait a generous amount of blocks between calls
            wrap m   = RetOk <$ m <* delay waitSlots    -- TODO catch errors

    shrinkAction _ Delay = []
    shrinkAction _ _     = [Action Delay]

    monitoring (_, s) _ _ _ = counterexample (show s)

delay :: Int -> ActionMonad PrismModel ()
delay n = void $ Trace.waitNSlots $ fromIntegral n

finalPredicate :: PrismModel -> TracePredicate
finalPredicate s = walletFundsChange issuer ada .&&.
                   walletFundsChange user (inv ada <> coin) .&&.
                   walletFundsChange mirror mempty .&&.
                   walletFundsChange credentialManager mempty .&&.
                   logs
    where
        n    = numTokens * balance s
        ada  = Ada.lovelaceValueOf n
        coin = STO.coins stoData n
        logs | Broken <- isIssued s = pure True -- emulatorLog (const False) "the log"
             | otherwise            = pure True

prop_Prism :: Script PrismModel -> Property
prop_Prism script = propRunScript finalPredicate before script after
    where
        before = run $ do
            userH    <- Trace.activateContractWallet user contract
            mirrorH  <- Trace.activateContractWallet mirror contract
            managerH <- Trace.activateContractWallet credentialManager contract
            Trace.callEndpoint @"role" userH    UnlockSTO
            Trace.callEndpoint @"role" mirrorH  Mirror
            Trace.callEndpoint @"role" managerH CredMan
            delay 5
            return initialState{ userHandle = Handle userH, mirrorHandle = Handle mirrorH, managerHandle = Handle managerH }
        after _ = return ()


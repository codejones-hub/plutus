{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Spec.Uniswap(tests) where

import           Control.Lens
import           Control.Monad                                    (guard, void)
import qualified Data.Map.Lazy                                    as Map
import           Data.Maybe                                       (fromJust)
import           Data.Semigroup                                   (Last (..))

import           Language.Plutus.Contract
import           Language.Plutus.Contract.Test
import           Ledger                                           (Ada, Value, getIndex, pubKeyHash, txOutAddress, txOutValue)
import qualified Ledger.Ada                                       as Ada

import qualified Language.Plutus.Contract.StateMachine            as SM
import qualified Language.PlutusTx.AssocMap                       as AMap
import           Language.PlutusTx.Coordination.Contracts.Auction
import           Language.PlutusTx.Coordination.Contracts.Uniswap
import           Language.PlutusTx.Monoid                         (inv)
import qualified Ledger.Value                                     as Value
import qualified Plutus.Trace.Emulator                            as Trace
import           Plutus.V1.Ledger.Address
import           Wallet.Emulator.Chain                            as Wallet

import           Test.Tasty

tests :: TestTree
tests =
    testGroup "uniswap"
        [ checkPredicateOptions options "start the Uniswap factory"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone buyer (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState buyer (Trace.walletInstanceTag w2) ((==) (Just $ Last trace1FinalState)) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace1WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace1WinningBid) <> theToken))
            startTrace
        , checkPredicateOptions options "run an auction"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone buyer (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertAccumState buyer (Trace.walletInstanceTag w2) ((==) (Just $ Last trace1FinalState)) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace1WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace1WinningBid) <> theToken))
            auctionTrace1
        , checkPredicateOptions options "run an auction with multiple bids"
            (assertDone seller (Trace.walletInstanceTag w1) (const True) "seller should be done"
            .&&. assertDone buyer (Trace.walletInstanceTag w2) (const True) "buyer should be done"
            .&&. assertDone buyer (Trace.walletInstanceTag w3) (const True) "3rd party should be done"
            .&&. assertAccumState buyer (Trace.walletInstanceTag w2) ((==) (Just $ Last trace2FinalState)) "final state should be OK"
            .&&. walletFundsChange w1 (Ada.toValue trace2WinningBid <> inv theToken)
            .&&. walletFundsChange w2 (inv (Ada.toValue trace2WinningBid) <> theToken)
            .&&. walletFundsChange w3 mempty)
            auctionTrace2
        ]

params :: AuctionParams
params =
    AuctionParams
        { apOwner   = pubKeyHash $ walletPubKey (Wallet 1)
        , apAsset   = theToken
        , apEndTime = 100

        }

coinA, coinB :: Coin
coinA = Coin "66" "A"
coinB = Coin "66" "B"

flattenValue :: Value -> [(Value.CurrencySymbol, Value.TokenName, Integer)]
flattenValue v = do
    (cs, m) <- AMap.toList $ Value.getValue v
    (tn, a) <- AMap.toList m
    guard $ a /= 0
    return (cs, tn, a)

startTrace :: Trace.EmulatorTrace ()
startTrace = do
    let w1 = Wallet 1
    hd <- Trace.activateContractWallet w1 start
    Trace.callEndpoint @"start" hd ()
    void $ Trace.waitUntilSlot 5

myTrace :: Trace.EmulatorTrace ()
myTrace = do
    let w1 = Wallet 1
        w2 = Wallet 2
        w3 = Wallet 3


    hd <- Trace.activateContractWallet w1 start

    Trace.callEndpoint @"start" hd ()
    void $ Trace.waitUntilSlot 5
    us <- uniswap . cCurrency . fromJust <$> findUniswapCoin

    hd1 <- Trace.activateContractWallet w1 $ userEndpoints us
    hd2 <- Trace.activateContractWallet w2 $ userEndpoints us
    hd3 <- Trace.activateContractWallet w3 $ userEndpoints us

    Trace.callEndpoint @"create" hd2 $ CreateParams coinA coinB 500000 1000000
    void $ Trace.waitUntilSlot 10

    Trace.callEndpoint @"swap" hd1 $ SwapParams coinA coinB 1000 0
    void $ Trace.waitUntilSlot 15

    Trace.callEndpoint @"add" hd3 $ AddParams coinA coinB 500000 1000000
    void $ Trace.waitUntilSlot 20

    Trace.callEndpoint @"swap" hd1 $ SwapParams coinB coinA 1990 0
    void $ Trace.waitUntilSlot 25

    Trace.callEndpoint @"remove" hd2 $ RemoveParams coinA coinB 707107
    void $ Trace.waitUntilSlot 30

    Trace.callEndpoint @"close" hd3 $ CloseParams coinA coinB
    void $ Trace.waitUntilSlot 35

findUniswapCoin :: Trace.EmulatorTrace (Maybe Coin)
findUniswapCoin = do
    s <- Trace.chainState
    let xs :: [Coin]
        xs = do
            (_, o) <- Map.toList $ getIndex $ Wallet._index s
            case txOutAddress o of
                ScriptAddress _ -> do
                    (cs, tn, _) <- flattenValue $ txOutValue o
                    guard $ tn == "Uniswap"
                    return $ Coin cs tn
                _ -> []
    return $ case xs of
        [c] -> Just c
        _   -> Nothing

-- | The token that we are auctioning off.
theToken :: Value
theToken =
    -- "ffff" is not a valid MPS hash. But this doesn't matter because we
    -- never try to forge any value of "ffff" using a script.
    -- This currency is created by the initial transaction.
    Value.singleton "ffff" "token" 1

-- | 'CheckOptions' that inclues 'theToken' in the initial distribution of wallet 1.
options :: CheckOptions
options =
    let initialDistribution = defaultDist & over (at (Wallet 1) . _Just) ((<>) theToken)
    in defaultCheckOptions & emulatorConfig . Trace.initialChainState .~ Left initialDistribution

seller :: Contract (Maybe (Last AuctionState)) SellerSchema SM.SMContractError ()
seller = auctionSeller (apAsset params) (apEndTime params)

buyer :: Contract (Maybe (Last AuctionState)) BuyerSchema SM.SMContractError ()
buyer = auctionBuyer params

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

trace1WinningBid :: Ada
trace1WinningBid = 50

auctionTrace1 :: Trace.EmulatorTrace ()
auctionTrace1 = do
    _ <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 buyer
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 trace1WinningBid
    void $ Trace.waitUntilSlot (succ $ succ $ apEndTime params)

trace2WinningBid :: Ada
trace2WinningBid = 70

auctionTrace2 :: Trace.EmulatorTrace ()
auctionTrace2 = do
    _ <- Trace.activateContractWallet w1 seller
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 buyer
    hdl3 <- Trace.activateContractWallet w3 buyer
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"bid" hdl2 50
    _ <- Trace.waitNSlots 15
    Trace.callEndpoint @"bid" hdl3 60
    _ <- Trace.waitNSlots 35
    Trace.callEndpoint @"bid" hdl2 trace2WinningBid
    void $ Trace.waitUntilSlot (succ $ succ $ apEndTime params)

trace1FinalState :: AuctionState
trace1FinalState =
    Finished $
        HighestBid
            { highestBid = trace1WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }

trace2FinalState :: AuctionState
trace2FinalState =
    Finished $
        HighestBid
            { highestBid = trace2WinningBid
            , highestBidder = pubKeyHash (walletPubKey w2)
            }

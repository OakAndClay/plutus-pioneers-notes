{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Void                  (Void)
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import GHC.Generics               (Generic)
import Ledger
import Ledger.Ada                 as Ada
import Ledger.Constraints         as Constraints
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = BlockchainActions .\/ Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract2

payContract2 :: Contract () PaySchema Text ()
payContract2 = Contract.handleError 
    (\err -> Contract.logError $ "Caught error: " ++ unpack err)
    payContract


-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.

payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h1 <- activateContractWallet (Wallet 1) payContract
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = x
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = y
        }
    void $ Emulator.waitNSlots 1

payTrace2 :: Integer -> Integer -> EmulatorTrace ()
payTrace2 x y = do
    h1 <- activateContractWallet (Wallet 1) payContract2
    callEndpoint @"pay" h1 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = x
        }
    void $ Emulator.waitNSlots 1
    h2 <- activateContractWallet (Wallet 1) payContract2
    -- Had to activate a second contract wallet because the first one was shutting down after the failed transaction.
    callEndpoint @"pay" h2 $ PayParams
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace  = y
        }
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace2 1000000000 2000000

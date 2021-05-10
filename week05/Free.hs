{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Free where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- We need to add the inlinable for mkPolicy to compile inside the oxford brackets.
{-# INLINABLE mkPolicy #-}
-- much like a validator scrypt but it only has ScriptContext as a field and it returns a Bool.
mkPolicy :: ScriptContext -> Bool
mkPolicy _ = True
-- This is the minting policy that allows arbitrary minting and burning of tokens.

policy :: Scripts.MonetaryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
-- We use this to get the CurrencySymbol from the policy.
-- This completes the on-chain part of the script.


data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema =
    BlockchainActions
        .\/ Endpoint "mint" MintParams
-- one of the parameters of the contract monad is the Schema
-- It defines the available actions that we can take
-- .\/ defines the endpoint.
-- if we use this schema as a parameter for our contract we will be able to use the mint endopoint.


-- 26:10
mint :: MintParams -> Contract w FreeSchema Text ()
-- Contract takes 4 type parameters
-- First one is the type of status that we tell
mint mp = do
    let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
    -- this identifies the value we want to forge
        lookups = Constraints.monetaryPolicy policy
        --
        tx      = Constraints.mustForgeValue val
        -- construct and submit transactions
        -- we define constrains to define the required parameters of the transaction.
        -- these conditions all start with 'must'
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    -- this is where the actual transaction takes place
    -- looks for one or more UTXOs to cover the transaction and applies it to the transaction
    -- submitTxConstraintsWith can fail if there are insuffiecient funds, etc.
    -- when submitTxConstraintsWith sees the mustForgeValue constraint it knows that it needs to forge/burn so it looks for the policy minting script.
    -- submitTxConstraintsWith finds the policy minting script through lookups.
    -- there are different versions of submitTxConstraintsWith that don't need an input
    void $ awaitTxConfirmed $ txId ledgerTx
    -- this confirms the transaction
    -- if submitTxConstraintsWith fails it will never make it to this line. It would return a nothing.
    Contract.logInfo @String $ printf "forged %s" (show val)

-- endpoints is the name of the contract that the playground will run.
endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    callEndpoint @"mint" h2 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 444
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = -222
        }
    void $ Emulator.waitNSlots 1

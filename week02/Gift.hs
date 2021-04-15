{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract     hiding (when)
import           PlutusTx            (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (Semigroup (..))
import           Text.Printf         (printf)

{-# INLINABLE mkValidator #-}
-- This allows mkValidator to be used within the $$()
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
-- this defines what the validator needs to allow the transaction. It doesn't need anything in this example.
-- Any wallet can grab the funds without any requirements.

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
-- This is Template Haskell
--      - An advanced feature of haskell
--      - Code generating code
--      - It is a Macro system - it gets expanded at compile time
--      - You don't have to understand template haskell to compile. basically copy paste
--      - $$ - is a splice. It inserts a script
--      - [|| Oxford Brackets||]

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
-- uses a function in scripts called validatorHash and it takes the validator as the input

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
-- this creates a script address by applying the ScriptAddress constructor to the product of the valHash
-- this is not a public key, this is a plutus script address
-- 67129f0ae65aa04bf99ebe459d9a36d1ea3ad1cafc2f9d4a1ef2da8891007afb
-- This is what it lools like. It is a hash of the valHash

-- the above code is boiler plate standard for most contracts

type GiftSchema =
    BlockchainActions
        .\/ Endpoint "give" Integer
        -- gives takes an integer: the amount of lovelace that the giver want's to send
        .\/ Endpoint "grab" ()
        -- looks for utxo at the address and takes what is available

give :: (HasBlockchainActions s, AsContractError e) => Integer -> Contract w s e ()
give amount = do
    -- Give takes in an integer, that's it.
    let tx = mustPayToOtherScript valHash (Datum $ Constr 0 []) $ Ada.lovelaceValueOf amount
    -- mustPayToOtherScript recieves valHash, a datum that doesn't matter, and the amount, an integer converted to lovelace.
    -- Datum takes data and turns it to a datum
    --  -can take any amount here. It doesn't matter. "Constr 0 []" is meaningless. Lars picked it for no reason.
    -- $ is a function application opperator (LHGG p. 81 - Chapter 5 - Function Application with $)
    ledgerTx <- submitTx tx
    -- tx is defined above
    --  -it is applied to submitTx 
    --  -submitTx is an I/0 action 
    --  - "<-" can be used in do syntax to bind I/0 values to a name (LHGG p. 218). In this case ledgerTx
    void $ awaitTxConfirmed $ txId ledgerTx
    -- ledgerTx is applied to txId then to awaitTxConfrmed which returns a value to void
    logInfo @String $ printf "made a gift of %d lovelace" amount
    -- after tx is confirmed ti prints this to screen

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    utxos <- utxoAt scrAddress
    -- uses the script address in a function called utxoAt to see what unspent utxos are at the address.
    -- utxoAt needs the address to see what utxo's are there.
    let orefs   = fst <$> Map.toList utxos
    -- (fst <$> Map.toList utxos) = (fmap fst (Map.toList utxos)) 
    -- (LLGG p. 231 - Chapter 11 - The Applicative Style)
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
        -- devines the trasaction by telling it that it must deliver all the unspent utxo's at the script address.
        -- The redeemer doesn't matter so Lars just sent I 17
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

-- each wallet has a choice between give and grab as often as they want.

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []

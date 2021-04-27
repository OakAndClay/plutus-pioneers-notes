-- Video Reference
-- https://www.youtube.com/watch?v=Lk1eIVm_ZTQ&t=3418s
-- Starts @ 27:50

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

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

-- 29:05
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: Slot
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- 30:10 - Redeemer
-- We dont need any info in the redeemer. All the info we need is in the context.
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
-- Pass the new type 'VestingDatum' into mkValidator as the Datum
-- 30:54 - Check Conditions
mkValidator dat () ctx =
    traceIfFalse "beneficiary's signature missing" checkSig      &&
    -- Make sure the beneficiary has access by checking signature included in transaction
    traceIfFalse "deadline not reached"            checkDeadline
    -- Make sure this is executed after deadline is reached.

-- 33:10
-- Reference Contexts.hs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    -- info is daty type TxInfo and we pass ctx from mkValidator into scriptContextTxInfo 
    -- which is also data type TxInfo

-- 34:33    
    checkSig :: Bool
    checkSig = beneficiary dat `elem` txInfoSignatories info
    -- "txInfoSignatories" is a list of pubkey hashes stored in a field in info
    -- we are checking to see if the beficiaries pubkey hash is included in this list
    -- "elem" is a standard haskell helper funciong it checks if an element is included
    -- in a list
    -- writing it `elem` allows us to write it inline making it feal more natural

-- 36:10
    checkDeadline :: Bool
    checkDeadline = from (deadline dat) `contains` txInfoValidRange info
    -- txInfoValidRange field from info gives us a slot range
    -- `contains` is an interval helper funcion from Interval.hs, 
    -- The current transaction slot is included in the range. checkDeadline makes sure 
    -- that all slots in ValidRange are after the deadline
    -- "from" is also an interval helper function. It says that the range of a valid transaction
    -- begins at the deadline field that the user provides in dat

-- 39:10
-- The rest is making sure that the boiler plate parameters match our mkValidator

data Vesting -- Name that makes sense
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = VestingDatum -- Matches Datum
    type instance RedeemerType Vesting = () -- Matches Redeemer

inst :: Scripts.ScriptInstance Vesting
inst = Scripts.validator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- 40:20
data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter (isSuitable pkh now) <$> utxoAt scrAddress
    if Map.null utxos
        -- if no utxo is available "no gifts available"
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                          mustValidateIn (from now)
                          -- if we dont do this the transaction will use the always slot range by default 
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
-- 41:15
  where
    isSuitable :: PubKeyHash -> Slot -> TxOutTx -> Bool
    isSuitable pkh now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []

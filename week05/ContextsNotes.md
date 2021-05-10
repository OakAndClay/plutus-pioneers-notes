* Validation Rehash
  * If
    * Don't have a public key address
    * Do have script address
    * Do have a UTXO
    * So have a transaction that tries to consume the UTXO as an imput
  * Then
    * For each script input the corresponding script will run
    
  * Script takes inputs
    * Datum
    * Redeemer
    * Context
    
## ScriptContext 
is defined in [Contexts.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs)
```
data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
```
ScriptContext has two fields that are defined
### TxInfo
```
-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoInputsFees  :: [TxInInfo]     -- ^ Transaction inputs designated to pay fees
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoForge       :: Value -- ^ The 'Value' forged by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: SlotRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic)
```
__TxInfo__ contains all the context info about the transaction that is being validated.
* The minting policy is activated when the __txInfoForge__ field contains a non-zero number.
  * This value has been zero in everything that we have done so far.
  * If this is non-zero it can contain a "bag of asset classes". Different currency symbols and token names.
    * For each currency symbol that is contained in this value being forged.
      * Each currency symbol is the hash of a minting policy script
      * Each minting policy script is executed

### ScriptPurpose
```
-- | Purpose of the script that is currently running
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
```
Everything that we have seen so far uses the Spending TxOutRef purpose. TxOutRef is a refererence to the UTXO that the transaction is trying to consume.

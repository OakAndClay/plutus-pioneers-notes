# Notes

### Need three things to unlock a script address.
#### Datum
Can be a custom type as long as it implements the isData type class.
#### Redeemer
Can be a custom type as long as it implements the isData type class.
#### Context
This is of type Script Context. It used to be ValidatorCtx.

We will be looking more closely at the context this week.

https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Contexts.hs

##### Validation can happen in the wallet.
Transactions can fail when a UTXO has been consumed by another wallet. The transaction will fail without any fees.

A validation script should never run and fail. You can always run the script under the exact current conditions in the wallet and you can see that it would fail before you ever try to submit it.

##### txInfoValidRange :: SlotRange
This is a parameter in the TxInfo data type. It allows us to define valid time for transactions.

We need to be able to define the time at which a transaction can be validated.

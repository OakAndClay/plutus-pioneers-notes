
Defined in [Plutus.V1.Ledger](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Tx.hs)
*  TxOut
    *  A transaction output, consisting of a target address, a value, and optionally a datum hash.
```
data TxOut = TxOut {
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatumHash :: Maybe DatumHash
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON, NFData)
```


Defined in [Plutus.V1.Value.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs)
* Token Name
    * A wrapper around bytestring
```
{-# INLINABLE tokenName #-}
tokenName :: ByteString -> TokenName
tokenName = TokenName
```

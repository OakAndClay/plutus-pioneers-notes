## Case Study
### Fully Fledged Dapp
* Running on a moch chain
* Oracles
  * A way to use real world info for smart contracts running on the blockchain.
  * Often needed for smart contracts.
  * We are going to use one trusted data source and one type of data. ADA/USD
* There must be a UTXO for anything to happen on the blockchain.

* Validation only happens when you want to consume a UTXO at a script address not when you produce an output at a script address.
  * We need to distinguish between the true utxo oracle output and the infinite possible noise.
  * We can asign an NFT to an oracle address to insure the authenticity of an oracle. 

```
-- | A transaction output, consisting of a target address, a value, and optionally a datum hash.
data TxOut = TxOut {
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatumHash :: Maybe DatumHash
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON, NFData)
```

### Values

The original english auction contract was auctioning an NFT.
#### Relevant types defined in [plutus-ledger-api](https://github.com/input-output-hk/plutus/tree/master/plutus-ledger-api/src/Plutus/V1/Ledger)
* [Value.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs)

* The Map in Value is equivelant to mapping from an asset class (ByteString) to an integer.
```
newtype Value = Value { getValue :: Map.Map CurrencySymbol (Map.Map TokenName Integer) }
    deriving stock (Generic)
    deriving anyclass (ToJSON, FromJSON, Hashable, NFData)
    deriving newtype (Serialise, PlutusTx.IsData)
    deriving Pretty via (PrettyShow Value)
```
* ADA is one type of AssetClass. Custom native tokens are also asset classes.
```
newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.IsData, Serialise, Show)
    deriving anyclass (Hashable, NFData, ToJSON, FromJSON)
    deriving Pretty via (PrettyShow (CurrencySymbol, TokenName))
```

* All native assets includeing ADA are identified by a CurrencySymbol and a TokenName
  * Both are newtype wrappers around a ByteString
```
newtype CurrencySymbol = CurrencySymbol { unCurrencySymbol :: Builtins.ByteString }
    deriving (IsString, Show, Serialise, Pretty) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.IsData)
    deriving anyclass (Hashable, ToJSONKey, FromJSONKey,  NFData)

```
```
newtype TokenName = TokenName { unTokenName :: Builtins.ByteString }
    deriving (Serialise) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.IsData)
    deriving anyclass (Hashable, NFData)
    deriving Pretty via (PrettyShow TokenName)
```
Fire up the repl. `import Plutus.V1.Ledger.Ada` and also `:set -OverloadedStrings` which allows us to enter ByteStrings as literal strings. CurrencySymbol and TokenName implement the IsString class. This allows us to enter them as literal strings.

In the [Ledger.Ada](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs) module there are two functions adaSymbol and adaToken.
```
:t adaSymbol
adaSymbol :: CurrencySymbol
:t adaToken
adaToken :: TokenName
```
Both are empty ByteStrings.

There is a funcion that builds an amount of Ada it is `lovelaceValueOf`
```
:t lovelaceValueOf
lovelaceValueOf :: Integer -> Value
```

* [Ada.hs](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Ada.hs)

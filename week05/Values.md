### Values

The original english auction contract was auctioning an NFT.
#### Relevant types defined in [plutus-ledger-api](https://github.com/input-output-hk/plutus/tree/master/plutus-ledger-api/src/Plutus/V1/Ledger)
* [Ledger.Value](https://github.com/input-output-hk/plutus/blob/master/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs)

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
It takes an integer and  returns a value `lovelaceValueOf 123` will return a Value Type. `Value (Map [(,Map [("",123])])` The first Map has an empty space after, this is the CurrencySymbol for Ada. The "" after the second map is the TokenName of Ada. `lovelaceValueOf` is a [Monoid](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids). You can use `mappend` to combine monoids. There is an opperator that can do this. `lovelaceValueOf 123 <> lovelaceValueOf 10` this will return `Value (Map [(,Map [("",133])])`

How do we create value using native tokens?
```
import Plutus.V1.Ledger.Value
:t singleton
singleton :: CurrencySymbol -> TokenName 0> Integer -> Value
```
This will construct an AssetClass with an amount of that asset. `singleton "a8ff" "ABC" 7` will return `Value (Map [(a8ff,Map [("ABC",7)])])`
We can mappend AssetClasses of different types into a single value `singleton "a8ff" "ABC" 7 <> lovelaceValueOf 43 <> singleton "a8ff" "XYZ" 100`. We can extract the ammount of a specific AssetClass from a Value using `valueOf`.
```
let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 43 <> singleton "a8ff" "XYZ" 100
:t valueOf
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf v "a8ff" "XYZ"
100
```
Another useful funciton is `flattenValue`it is of type `flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]`.
```
flattenValue v
[(a8ff,"ABC",7),(a8ff,"XYZ",100),(,"",43)]
```
#### Why do we need a CurrencySymbol and a TokenName?
* Minting Policies
  * A transaction can not create or delete tokens.
  * The reason the CurrencySymbol is hexidecimal.
     * It is the hash of the mintingpolicy script
     * If we have a trasnaction where we want to create or burn tokens then for each native token the CurrencySymbol is looked up.
     * The CurrencySymbol points to the mintingpolicy script that is included in the transaction.
     * The mintingpolicy script determines if the transaction has a right to mint or burn tokens.
  * Ada has no CurrencySymbol hash because there is no mintingpolicy script. No way to mint or burn Ada.

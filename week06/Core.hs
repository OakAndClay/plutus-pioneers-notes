{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , oracleInst
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..))
import qualified Prelude                   as Prelude

-- Paramaterized contract
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol -- NFT currency symbol. The token name is just an empty string
    , oOperator :: !PubKeyHash     -- The opperator that can make updates
    , oFee      :: !Integer        -- Fees in lovlace that are due each time it is used
    , oAsset    :: !AssetClass     -- This is the asset class of the other asset ADA/USDT
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord) -- These are boiler plate type classes to make the type liftable

PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use -- Supporting two operations
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer -- Use template haskell to implement IsData

{-# INLINABLE oracleTokenName #-} -- Need this to use the TokenName as an empty string for the NFT
oracleTokenName :: TokenName
oracleTokenName = TokenName emptyByteString

{-# INLINABLE oracleAsset #-} -- This defines the NFT AssetClass for the Oracle. It takes in the oSymbol field from Oracle.
oracleAsset :: Oracle -> AssetClass
oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
oracleValue o f = do        -- do monadic notation makes this nice and clean
    dh      <- txOutDatum o -- o is a TxOut that will return nothing if it doesnt have a Datum otherwise it will produce a datum hash dh
    Datum d <- f dh         -- f is a provided funcion that maybe turn the datum hash to a Datum (type wrapper around data)
    PlutusTx.fromData d     -- PlutusTx will Maybe turn the Datum into an integer.

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ oOperator oracle) && -- has the opperator signed the transaction
                  traceIfFalse "invalid output datum"       validOutputDatum                        -- Does it cary valid data
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              && -- Chacks that the value of the oracle hasn't changed
                  traceIfFalse "fees not paid"              feesPaid                                -- Have the fees been paid?
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut -- The oracle output
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> txInInfoResolved i -- i is of type txinput

    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (oracleAsset oracle) == 1 -- assetClassValueOf is in ledger.module
    -- takes an asset class and value and returns an integer. how many coins are included in that value. Checking if the NFT is there.

    ownOutput :: TxOut -- 
    ownOutput = case getContinuingOutputs ctx of -- getContinuingOutputs returns a list of all the outputs that go to the scirpt address that is being validated.
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (oracleAsset oracle) == 1 -- checking to see if the oracle output contains the NFT

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`findDatum` info) -- takes info and datum hash and tries to look up the corrisponding datum

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = txOutValue ownInput
        outVal = txOutValue ownOutput
      in
        outVal `geq` (inVal <> Ada.lovelaceValueOf (oFee oracle)) -- output should be equal to the input value plus the fees. `geq` is greater than or equal to.

-- The above is the core buisiness logic of the oracle

data Oracling
instance Scripts.ScriptType Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

oracleInst :: Oracle -> Scripts.ScriptInstance Oracling
oracleInst oracle = Scripts.validator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> Validator
oracleValidator = Scripts.validatorScript . oracleInst

oracleAddress :: Oracle -> Ledger.Address
oracleAddress = scriptAddress . oracleValidator

-- The below is the off-chain code

data OracleParams = OracleParams
    { opFees   :: !Integer
    , opSymbol :: !CurrencySymbol -- This is the information of the asset we want to track. Not the NFT.
    , opToken  :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON)

startOracle :: forall w s. HasBlockchainActions s => OracleParams -> Contract w s Text Oracle -- The only thing we are doing in this function is minting the NFT.
startOracle op = do -- The minting can take a couple slots so keeping it separate from the oracle data will allow the date to be more accurate.
    pkh <- pubKeyHash <$> Contract.ownPubKey
    osc <- mapError (pack . show) (forgeContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency) -- forgeContract allows us to mint NFTs
    let cs     = Currency.currencySymbol osc
        oracle = Oracle
            { oSymbol   = cs
            , oOperator = pkh
            , oFee      = opFees op
            , oAsset    = AssetClass (opSymbol op, opToken op)
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let c = Constraints.mustPayToTheScript x $ assetClassValue (oracleAsset oracle) 1
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (oracleInst oracle) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.scriptInstanceLookups (oracleInst oracle) <>
                          Constraints.otherScript (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x

findOracle :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle = do
    utxos <- Map.filter f <$> utxoAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, x)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (oracleAsset oracle) == 1

type OracleSchema = BlockchainActions .\/ Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle

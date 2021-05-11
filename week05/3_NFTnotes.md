### NFTs

* An NFT is a unique token. There can be only one.
* We can use a deadline to ensure that there can not be another token minted after this epoch passes.
  * This is only possible with Plutus.
  * Currencly NFTs need to be verified with a block chain explorer
    * How easy is it to counterfit?
* We can also use transactions, UTXOs as a unique identifying mark.
  * an empty transaction without fees would have a replicatable transaction hash.
  * Cardano has fees so it produces unique transaction id.
  * Every transaction and UTXO has a unique id

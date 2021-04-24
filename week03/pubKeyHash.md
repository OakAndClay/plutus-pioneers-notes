https://www.youtube.com/watch?v=Lk1eIVm_ZTQ&t=2445s
45:15

#### Getting the pubKeyHash for plutus playground

##### Fire up cabal repl
```
cd ~/plutus
sudo nix-shell
cd ..
cd plutus-pioneer-program/code/week03
cabal repl
inport Wallet.Emulator
import Ledger
pubKeyHash $ walletPubKey $ Wallet 2
```
Prelude Returns:
39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f

```
Prelude Wallet.Emulator Ledger Week03.IsData> :t walletPubKey
walletPubKey :: Wallet -> PubKey
Prelude Wallet.Emulator Ledger Week03.IsData> :t pubKeyHash
pubKeyHash :: PubKey -> PubKeyHash
Prelude Wallet.Emulator Ledger Week03.IsData> :i Wallet
type Wallet :: *
newtype Wallet = Wallet {getWallet :: Integer}
```

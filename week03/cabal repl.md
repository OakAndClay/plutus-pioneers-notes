Fire up the repl
```
cd ~/plutus
nix-shell
cd .. && cd plutus-pioneer-program/code/week03
cabal repl
```

Import the relevant files

```
import Plutus.V1.Ledger.Slot
import Plutus.V1.Ledger.Interval
```
2 ways to define a slot
```
Slot 3
3 :: Slot
```
Prelude will return
```
Slot {getSlot = 3}
```

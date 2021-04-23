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
Prelude returns:
```
Slot {getSlot = 3}
```
Lets use a interval helper function to construct an interval.
```
interval (Slot 3) 10
```
Prelude returns:
```
Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 3})) True, ivTo = UpperBound (Finite (Slot {getSlot = 10})) True}
```
ivFrom: The beginning of the interval
Finite: Indicates that it is a specific time, 3. True indicates that 3 is included.

Check if 5 is included in the SlotRange
```
member 5 $ interval (Slot 3) 10
```
Prelude returns:
```
True
```
We can use member to test the range of the interval by plugging different numbers into member and checking to see if the result is what we expect.

Lets try the from funcion
```
from (Slot 20)
```
Prelude returns:
```
Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 20})) True, ivTo = UpperBound PosInf True}
```
Lower bound is a finite number, 20 and it is included in the range. The upper boud is possitive infinity and it is included in the range.



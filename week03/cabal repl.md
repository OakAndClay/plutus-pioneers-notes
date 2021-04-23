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
#### 2 ways to define a slot
```
Slot 3
3 :: Slot
```
Prelude returns:
```
Slot {getSlot = 3}
```
#### interval s s'
```
interval (Slot 3) 10
```
Prelude returns:
```
Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 3})) True, ivTo = UpperBound (Finite (Slot {getSlot = 10})) True}
```
ivFrom: The beginning of the interval
Finite: Indicates that it is a specific time, 3. True indicates that 3 is included.

#### member a i
```
member 5 $ interval (Slot 3) 10
```
Prelude returns:
```
True
```
We can use member to test the range of the interval by plugging different numbers into member and checking to see if the result is what we expect.

#### from s
```
from (Slot 20)
```
Prelude returns:
```
Interval {ivFrom = LowerBound (Finite (Slot {getSlot = 20})) True, ivTo = UpperBound PosInf True}
```
Lower bound is a finite number, 20 and it is included in the range. The upper boud is possitive infinity and it is included in the range.

#### to s
```
to (Slot 100)
```
Prelude returns:
```
Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound (Finite (Slot {getSlot = 100})) True}
```
The range starts including(True) negative infinity(NegInf) and extends to a finite number, 100.

```
member (-124523) $ to (Slot 100)
```
Prelude returns:
```
True
```
#### contains (Interval l1 h1) (Interval l2 h2)
```
contains (to $ Slot 100) $ interval 30 50
```
Prelude returns:
```
True
```
contains checks to see if the second interval falls within the bounds of the first. The code looks like this.
```
contains (Interval l1 h1) (Interval l2 h2) = l1 <= l2 && h2 <= h1
```


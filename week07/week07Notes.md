## State Machine

A **state machine** is a system that allows you to transition from one state to another. You can go back and forth between different states multiple times. States can also be defined as **final states**. The state can no longer change after it reaches a final state.

![State1](/week07/images/State1.png)

**The first state** in this example is where the first player has made a move. **The state is characterized by a datum hash**.

There are *two* possible states coming from this position. **Player 2** (Bob) can choose to **play**, *otherwise* **Player 1** (Alice) can **reclaim**. 

* Notes in this diagram represent **states**.
  * *States* on the blockchain are represented by **UTXOs**
  * These UTXOs occupy a state machine script address.
  * The *state* of the state machine is the **Datum** of the *UTXO*
* *Arrows* are the **transitions** between the states.
  * These *trasitions* are **Transactions** on the blockchain that *consume* the **UTXO**
  * The *transitions* are characterized by a **Redeemer**
  * This transitions produces a **new UTXO** at the *same script address* with a **new Datum** that defines the current state.

The **support** for for **state machines** can be found at [plutus-contract/src/Plutus/Contract/StateMachine/OnChain.hs](https://github.com/input-output-hk/plutus/blob/1b6dedf0b9eca7df02bf34d71de94af7549ddc80/plutus-contract/src/Plutus/Contract/StateMachine/OnChain.hs)

![State2](/week07/images/State2.png)

A state machine is a record type with 4 fields. The most important being the transition.
```
data StateMachine s i = StateMachine {smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s)
```
s = state = datum type `State s`
```
data State s = State { stateData :: s, stateValue :: Value }
```
`stateData` is the state type datum and `stateValue` is the value of the UTXO

i = input = redeemer type
 * It is the transaction that tries to consume the UTXO

`Maybe` alows us to return nothing. Incicating that this function has the possibility of not being allowed.


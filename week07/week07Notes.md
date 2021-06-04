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

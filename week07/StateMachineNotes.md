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

```
data StateMachine s i = StateMachine 
      {
      smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s),
      smFinal       :: s -> Bool,
      smCheck       :: s -> i -> ScriptContext -> Bool,
      smThreadToken :: Maybe AssetClass
      }
```
`StateMachine` takes two imputs `s i`
* `s` = state = datum type
* `i` = input = redeemer type. It is the transaction that tries to consume the UTXO `s`

`StateMachine` is a record type with 4 fields `smTransition` `smFinal` `smCheck` `smThrreadtoken`. The most important being the `smTransition`.

`smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s)`
* `State s` is a UTXO. It is defined as `data State s = State { stateData :: s, stateValue :: Value }`
  * `stateData` = datum of the UTXO
  * `stateValue` = value of the UTXO
* `Maybe` allows us to return nothing. Indicating that this function has the possibility of failing. If it succeeeds, it will return a tuple. 
  * `(TxConstraints Void Void, State s)`.
    * `State s`, Includeing the new datum and value of the UTXO.
    * `TxContraints` specifies additional constraints the transition `i` that produces this new UTXO must satisfy.

`smFinal :: s -> Bool` Identifies final states. There is no value attached with a final state and it does not produce a new UTXO

`smCheck :: s -> i -> ScriptContext -> Bool` It recieves the datum, redeemer and context and returns a Bool.

`smThreadToken :: Maybe AssetClass` Identifies the currect UTXO that is sitting at the address of the StateMachine. `AssetClass` would be an NFT used as a unique identifier for the UTXO.


### Example Code
We are going to take a look at a version of the [EvenOdd.hs](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week07/src/Week07/EvenOdd.hs) game written using StateMachine. It is called [StateMachine.hs](https://github.com/input-output-hk/plutus-pioneer-program/blob/main/code/week07/src/Week07/StateMachine.hs).

Let's take a look at some of the functions for StateMachine.hs

`GameDatum`
* includes a second constructor called `Finished` to represent the final state of the state machine. 
  * It will not include a UTXO. 
  * We need it for the state machine to work.
* The definition of equality for `GameDatum` needs to include the `Finished` constructor.
```
data GameDatum = GameDatum ByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False    
```

`transition`
```
{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)       <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)   <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game) <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing
  where
    token :: Value
    token = assetClassValue (gToken game) 1
```


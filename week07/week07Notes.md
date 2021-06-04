## State Machine
![State1](/week07/images/State1.png)
A state machine is a system that allows you to transition from one state to another. You can go back and forth between different states multiple times. States can also be defined as **final states**. The state can no longer change after it reaches a final state.

The first state in this example is where the first player has made a move. The state is characterized by a datum hash.

There are two possible states coming from this position. Player 2 (Bob) can choose to play. Otherwise Player 1 (Alice) can reclaim. All the notes in this diagram represent states and the arrows are the transitions between the states.

On the blockchain a state machine is represented by a UTXO that occupies a state machine script address. The state of the *state* machine is the *Datum* of that UTXO.

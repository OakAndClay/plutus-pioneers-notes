# plutus-pioneers-notes
## Getting Started

After building nix inside the plutus repo we can launch nix-shell within that directory. Open a terminal.

` cd ~/plutus && sudo nix-shell `

Then we can launch the plutus server.

``` 
cd ~/plutus/plutus-playground-client
plutus-playground-server 
``` 

Open another terminal tab and activate another nix shell. Then lauch the client.

```
cd ~/plutus && sudo nix-shell
cd plutus-playground-client
npm run start
```

Now we can open a browser and go to https://localhost:8009 to access Plutus Playground.

Open another terminal tab and launch a prelude terminal.

```
cd plutus && sudo nix-shell
cd /home/[user]/plutus-pioneer-program/code/[week]
cabal repl
```
### Personal Preferences

I like to use VScode to look at the contract files that are included in the https://github.com/input-output-hk/plutus-pioneer-program

I am following along with lars in the video and makeing comments in the code as notes. I will save those notes in this repo under the week of the course.

Using http://learnyouahaskell.com/chapters as a reference where I don't understand specifics in the code.


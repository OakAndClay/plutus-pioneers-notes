# plutus-pioneers-notes
## Getting Started

After building nix inside the plutus repo we can launch nix-shell within that directory. Open a terminal.

` cd ~/plutus && sudo nix-shell `

Then we can launch the plutus server.

` cd ~/plutus/plutus-playground-client && plutus-playground-server ` 

Open another terminal tab and activate another nix shell. Then lauch the client.

```
cd ~/plutus && sudo nix-shell && \
cd ~/plutus/plutus-playground-client && npm run start
```

Now we can open a browser and go to https://localhost:8009 to access Plutus Playground.

Open another nerminal tab and lauch a prelude terminal.

```
cd plutus && sudo nix-shell && \
cd /home/[user]/plutus-pioneer-program/code/[week] && cabal repl
```




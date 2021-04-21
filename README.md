# plutus-pioneers-notes
## Getting Started

After building nix inside the plutus repo we can launch nix-shell within that directory. Open a terminal.

` cd ~/plutus && sudo nix-shell `

Then we can launch the plutus server.

``` 
cd plutus-playground-client
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
cd ~/plutus && sudo nix-shell
cd /home/[user]/plutus-pioneer-program/code/[week]
cabal repl
```
### Personal Preferences

I like to use VScode to work with the contract files that are included in the https://github.com/input-output-hk/plutus-pioneer-program. 

I have installed two extensions that are great for Haskell.
https://marketplace.visualstudio.com/items?itemName=haskell.haskell
https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell

I am following along with lars in the video and makeing comments in the code as notes. I will save those notes in this repo under the week of the course.

I am using http://learnyouahaskell.com/chapters and http://book.realworldhaskell.org/read/ as references when I don't understand the code. I am citing this in the comments with the page number.

### Updating Plutus Dependancy

The Plutus depencies are constantly changing. It is in heavy acctive development. We need to keep the version that we are working with up to date.

```
cd ~/plutus
git fetch && git rebase
git checkout [current commit hash]
sudo nix build -f default.nix plutus.haskell.packages.plutus-core
```



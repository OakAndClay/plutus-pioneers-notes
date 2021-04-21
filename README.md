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

### Updating Plutus Dependancy

The Plutus depencies are constantly changing. It is in heavy acctive development. We need to keep the version that we are working with up to date.

This is a good resource for setting this up.
https://docs.plutus-community.com/docs/setup/Ubuntu.html

```
cd ~/plutus
git fetch && git rebase
git checkout [current commit hash]
sudo nix build -f default.nix plutus.haskell.packages.plutus-core
```
### Update the Plutus-Pioneers-Program Directory
```
cd ~/plutus-pioneers-program
git fetch && git rebase
git pull
```

### Personal Preferences

I like to use VSCode. https://code.visualstudio.com/Download

I have installed two Haskell extensions.
https://marketplace.visualstudio.com/items?itemName=haskell.haskell
https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell

VSCode is great for working with the code for the class. You can find the Plutus Pioneers github repo here. https://github.com/input-output-hk/plutus-pioneer-program.

I am following along with Lars in the video and makeing comments in the code as notes. I will save those notes in this repo under the week of the course.

I am using http://learnyouahaskell.com/chapters and http://book.realworldhaskell.org/read/ as references when I don't understand the code. I am citing this in the comments with the page number.


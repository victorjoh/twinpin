# twinpin
![twinpin screenshot](images/screenshot.png "twinpin screenshot")

twinpin is a minimal twin-stick shooter. The two players control their
characters with gamepads. So far I have only tried it with a Sony DualShock 4
V2. I develop this game to satisfy my functional programming itch.

## Build dependencies
Stack is used to build the project. Stack will download most of the other build
dependencies but there are some manual steps required to get SDL2 working.

### Windows specific setup
To set up SDL2 for Windows, follow step 1-4 in [this Reddit
post](https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/).

### Fedora specific setup
install SDL2 with `sudo dnf install SDL2 SDL-devel`

There is also some problem with the latest binutils release (26.fc31) causing
build problems. I got the error: 
```
corrupt GNU build attribute note: wrong note type: bad value
```

I had to upgrade binutils to the rawhide release 29.fc31. See [this
bug report](https://bugzilla.redhat.com/show_bug.cgi?id=1767937) for more
information. This can be done with the following commands:
```
sudo dnf install fedora-repos-rawhide -y
sudo dnf --disablerepo=* --enablerepo=rawhide --releasever=29 upgrade binutils --nogpgcheck
```

## How to build and play
`stack build` to build  
`stack exec twinpin-exe` to play twinpin, or  
`stack run` to both build and execute  
`stack test` to run the tests

## Suggested development environment
To develop twinpin, I use Visual Studio Code with the following extensions:
* Haskell Language Server (alanz.vscode-hie-server)
* Rewrap (stkb.rewrap)

Haskell Language Server has a requirement on Haskell IDE Engine. Build it with
the following commands:
```
sudo dnf install libicu-devel ncurses-devel
git clone https://github.com/haskell/haskell-ide-engine --recursive
cd haskell-ide-engine
stack ./install.hs build
```
This differs slightly from the installations instructions found in the [README
for Haskell Language
Server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server).
The `build-all` target is not available anymore. We don't want to build
everything anyway since it takes several hours and requires about 30 GB of disk
space. `build` only builds Haskell Language Server with the latests ghc instead
of all 8 available for the Haskell Language Server.

Cabal is needed for Haskell IDE Engine to work properly. To install and update
cabal, run
```
stack install cabal-install
cabal update
```
as written in [this github issue for Haskell IDE
Engine](https://github.com/haskell/haskell-ide-engine/issues/658).

# twinpin code architecture
The entry point is found in app/Main.hs. Main.hs contains all the IO. Main.hs
communicates with src/Game.hs. Game.hs is responsible for updating the game
state given some user input.
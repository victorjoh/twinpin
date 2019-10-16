# twinpin
![twinpin screenshot](images/screenshot.png "twinpin screenshot")

twinpin is a minimal twin-stick arena shooter. The two players control their
characters with gamepads. So far I have only tried it with a Sony DualShock 4
V2. I develop this game to satisfy my functional programming itch.

## Build dependencies
Stack is used to build the project. Stack will download most of the other build
dependencies but there are some manual steps required to get SDL2 working.

### Windows specific setup
To set up SDL2 for Windows, follow step 1-4 in [this Reddit
post](https://www.reddit.com/r/haskellgamedev/comments/4jpthu/windows_sdl2_is_now_almost_painless_via_stack/).

### Fedora specific setup
TODO

## How to build and play
`stack build` to build  
`stack exec twinpin-exe` to play twinpin  
`stack test` to run the tests

## Suggested development environment
To develop twinpin, I use Visual Studio Code with the following extensions:
* Haskell Language Server (alanz.vscode-hie-server)
* Rewrap (stkb.rewrap)

Haskell Language Server has a requirement on Haskell IDE Engine. Instruction for
how to install it is found in the [README for Haskell Language
Server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server).
It takes a few hours to build the complete Haskell IDE Engine.

### Windows specific setup
Cabal is needed for Haskell IDE Engine to work properly. To install and update
cabal, run
```
stack install cabal-install
cabal update
```
as written in [this github issue for Haskell IDE
Engine](https://github.com/haskell/haskell-ide-engine/issues/658).

### Fedora specific setup
TODO
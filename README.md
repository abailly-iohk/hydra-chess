# Games on Hydra

An experiment to build distributed and decentralised games running on top of Cardano Layer 2, [Hydra](https://hydra.family).
This project started with [Black Jack](https://en.wikipedia.org/wiki/Blackjack) and then switched to focus on _Chess_ which is a much more interesting game.

> [!WARNING]
> This project is a work-in-progress and experimental, it's not meant (yet?) to be used for real playing on mainnet and it's advised to only run it on test networks.
> The code follows a duct-tape-and-staples design strategy which basically means I hack stuff until they work just enough to make some progress, then move on. Furthermore, it suffers from premature generalisation as I attempted to decouple from the interaction with the execution and settlement layer too early.

## Status

* [x] Rules [in PlutusTx](./black-jack-core/src/Chess/Game.hs) test-driven in [Haskell](./black-jack-core/test/Chess/GameSpec.hs)
* [x] Barebones Console based interface
* [x] ~~Mock server simulating lifecycle on Hydra (for testing purpose)~~
* [x] Basic [Plutus](./black-jack-core/src/Chess/Contract.hs) smart contract
* [x] Create Cardano transactions using only [cardano-cli](https://github.com/IntersectMBO/cardano-cli)
* [x] Advanced smart contract: Check play is valid for any given game state
* [ ] ~~Proper "randomness"~~
* [x] Startup & configure Hydra server in the background
* [x] Startup & configure cardano-node in the background
* [x] Support for 2-players
* [ ] User guide
* [ ] Web UI

# Why?

The ability to play games in a safe and decentralised way is one
possible use case of blockchains. However, Layer 1 is way too
expensive and slow to enable a good gaming experience, hence the need
to rely on Layer 2 to speed things up and make it cheap and affordable
to play games with other people.

Having contributed to [Hydra](https://hydra.family) since its
inception, I also wanted to start experimenting with how one could
build such a system and application to make it reasonably easy to use
Hydra for gaming purpose.

The [Experience Report](./2023-experience-report.md) contains some
thoughts about Cardano, Hydra, DApps development, stemming from this
experiment.

# Black Jack on Hydra

An experiment to build a distributed and decentralised Black Jack game on top of Cardano Layer 2, [Hydra](https://hydra.family).

> :warning: **This project is a work-in-progress and experimental, it's not meant to be use for real playing on mainnet**

## Status

* [x] Basic rules [in Haskell](./black-jack-core/src/BlackJack/Game.hs) (should be easy to port to [Plutus](https://docs.cardano.org/plutus/learn-about-plutus))
* [x] Barebones Console based interface
* [x] Mock server simulating lifecycle on Hydra (for testing purpose)
* [x] Basic [Plutus]() smart contract (only checks proposed play is legit)
* [ ] Create Cardano transactions and play over a Cardano chain
* [ ] Advanced smart contract: Check play is valid for any given game state
* [ ] Proper "randomness"
* [ ] Hydra server (requires implementing multi-head hydra?)
* [ ] Web UI

# Why?

The ability to play games in a safe and decentralised way is one possible use case of blockchains. However, Layer 1 is way too expensive and slow to enable a good gaming experience, hence the need to rely on Layer 2 to speed things up and make it cheap and affordable to play games with other people. I wanted to start experimenting with how one could build such a system and application to make it reasonably easy to use Hydra for gaming purpose.

## Why Black Jack?

* [Black Jack](https://en.wikipedia.org/wiki/Blackjack) is totally uninteresting as far as game goes, with very limited strategy and decisions for the players
* Yet, it has rules, allow multiple players to participate in a single game, emphasized betting and monetary gains, and most importantly has an important element of _randomness_ which is particularly tricky to implement on a deterministic execution engine
* It's simpler than Poker

# Randomness

* By convention the party initiating the Head is the _dealer_ and represents the _Bank_: It is responsible for paying off the players' gains and dealing the cards while the game is running
* The _dealer_ plays mechanically, following the "Hit at 16, Stand at 17" rule
* It suffices the players to be ignorant of the cards distribution for the game to be "interesting"
* To generate a _seed_ for a PRNG,
  * The dealer generates a pair of VRF keys
  * At initialisation time, it generates a _nonce_ which is encrypted using the VRF signing key and committed on-chain along with the VRF proof
  * The nonce is privately XORed with the UTxO references of the players' committed outputs once they are knonw (eg. when the Head is opened)
  * This is the starting seed for the game which is updated using standard PRNG when dealing cards
* The current seed is always posted encrypted (with proof) alongside the game's state while the Game is running
* When the head is closed, the initial nonce is revealed and the whole sequence of seeds can be verified using the VRF proof

* This seed

## Links

* [VRF on Cardano](https://hackernoon.com/generating-randomness-in-blockchain-verifiable-random-function-ft1534ud)
* [VRF on Polkadot](https://wiki.polkadot.network/docs/learn-randomness)
* [RANDAO on Ethereum](https://soliditydeveloper.com/prevrandao)
* Some discussion on [randomness for games on blockchain](https://blog.logrocket.com/build-random-number-generator-blockchain/)

# Build

```
cabal update
cabal build all && cabal test all
```

# Usage

**TBD**

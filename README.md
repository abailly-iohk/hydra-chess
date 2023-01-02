# Black Jack on Hydra

An experiment to build a distributed and decentralised Black Jack game on top of Cardano Layer 2, [Hydra](https://hydra.family).

<div style="background-color:rgba(200, 0, 0, 0.5); text-align:center; vertical-align: middle; padding:40px 0;">
**This project is a work-in-progress and experimental, it's not meant to be use for real playing on mainnet**
</div>

## Status

* [x] Basic rules [in Haskell](./black-jack-core/src/BlackJack/Game.hs) (should be easy to port to [Plutus](https://docs.cardano.org/plutus/learn-about-plutus))
* [x] Barebones Console based interface
* [x] Mock server simulating lifecycle on Hydra (for testing purpose)
* [ ] Hydra server (requires implementing multi-head hydra)
* [ ] Web UI

# Why?

The ability to play games in a safe and decentralised way is one possible use case of blockchains. However, Layer 1 is way too expensive and slow to enable a good gaming experience, hence the need to rely on Layer 2 to speed things up and make it cheap and affordable to play games with other people. I wanted to start experimenting with how one could build such a system and application to make it reasonably easy to use Hydra for gaming purpose.

## Why Black Jack?

* [Black Jack](https://en.wikipedia.org/wiki/Blackjack) is totally uninteresting as far as game goes, with very limited strategy and decisions for the players
* Yet, it has rules, allow multiple players to participate in a single game, emphasized betting and monetary gains, and most importantly has an important element of _randomness_ which is particularly tricky to implement on a deterministic execution engine
* It's simpler than Poker

# Build

```
cabal update
cabal build all && cabal test all
```

# Usage

**TBD**

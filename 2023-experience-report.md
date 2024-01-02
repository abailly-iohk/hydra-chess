# Developing a Chess Game on Hydra

## Goals & Constraints

* The application should be easy to install and run, with basically
  zero configuration needed and very few knowledge about smart
  contracts, Cardano, layer 2, etc. from the part of the player(s)
* The client should be independent from the Cardano implementation and
  rely on generally available tools to interact with the layer 1 and
  2, eg. cardano-cli
* The client should be independent from Hydra implementation, and rely
  only on basic web API components: HTTP(S), Websockets, JSON
* The validity of game moves must be fully verified on-chain, making
  it impossible for a player to make an illegal move

## Easy install & zero-conf

* Installing a cardano-node is already a non-trivial process:
  * Binaries are only available for [some
    platforms](https://github.com/IntersectMBO/cardano-node/releases/tag/8.7.1-pre),
    and sometimes [not even
    unavailable](https://github.com/IntersectMBO/cardano-node/releases/tag/8.7.2)
  * One then has to unpack an archive comprising 10s of programs,
    libraries, and configuration files, and run it without much
    guidance (the archive does not contain a `README.md` or a user
    guide)
  * Another popular option is to build the cardano-node from source,
    either through
    [nix](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/building-the-node-using-nix.md)
    which is the recommended way, or [from
    "scratch"](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/install.md),
    but this requires a lot of time, disk space, CPU and RAM, and is
    quite an error-prone process
* Running a cardano-node on one of the public test networks or on
  `mainnet` is by contrast a straightforward process: Point the node
  to the publicly available configuration files and there you go
  * The only tricky part is getting the "right" configuration files:
    The canonical configuration files for the _latest_ version of the
    cardano-node and networks are available on the
    [cardano.org](https://book.world.dev.cardano.org/environments.html)
    website, but it's impossible to know from their URL which version
    they apply to
  * The situation has improved in recent releases as the configuration
    files are now packed with the archive, albeit with a different
    naming scheme
  * Also of note is the time needed to sync up a public network, a process that takes hours or days. This issue is easily solved with [Mithril](https://mithril.network) but unfortunately teh current snapshots are built for cardano-node 8.1.2 which still comprises the [majority of nodes](https://pooltool.io/networkhealth) in the network
* The situation is reversed for Hydra: Installing a hydra-node is
  simpler as there's only one important executable to install, namely
  the `hydra-node`, while configuring and running a hydra-node is
  quite an [involved
  process](https://hydra.family/head-protocol/docs/tutorial/). In
  particular:
  * One needs to have some funds ready at an address controlled by the hydra-node, which implies generating keys for both Cardano signing and Hydra signing
  * The hydra-node requires up-front knowledge about the parties it needs to connect to, their addresses and their keys
  * Also of importance is the transaction id relevant to the cardano network, in order to be able to post the Head protocol transactions (they are now [available in the code repository](https://raw.githubusercontent.com/input-output-hk/hydra/0.14.0/networks.json))

The [hygames](games-hydra/app/games-hydra.hs) executable aims at taking care of all those nitty-gritty details and provide a turnkey experience to the user which basically should be able to be up and running with a single command pointing to a given network, eg.:

```
hygames --network Preview
```

To this aim, the executable currently:
* Downloads executables and configuration files from reference sources according to version (when they are available)
* Uses XDG to manage installation directories, configurations, and data (eg. cardano-node database and hydra-node's state)
* Generates needed keys when they do not exist, possibly requesting funding from the user who can then use a standard wallet to send those funds to the hydra-node
* Wraps-up and control both executables, trying as much as possible to ensure they stay in sync

## The good

* The WS API is easy to work with and leads to straightforward reactive

## The ugly

* The Head commit process and interfaces are somewhat awkward to work with: The JSON schema is confusing, with various required/optional fields
* The use of websocket protocol to retrieve state from the hydra-node is awkward as one needs to "manually"synchronize on the responses
  * This should be addressed by [ADR-25](https://hydra.family/head-protocol/adr/25)
* The JSON format for UTxO is somewhat painful to work with because the keys are arbitrary strings representing various things: TxIns, policy ids, token names. It would be easier to have more classical data structure with lists of objects with standard field names
* The cardano-cli's documentation is not so great:
  * The inline documentation is barely usable, useful mostly to know the name of the flags to use, but is lacking a lot of details like for example the fact that inline datum provided on the command-line is supported only for builtin data types and not arbitrary ones
  * Online available documentation is very often inconsistent, lacunary, outdated, or a combination of those. The most useful pieces of documentation relative to plutus tokens minting and scripts inputs and outputs were hard to discover: The [Plutus Pioneer Program](https://github.com/input-output-hk/plutus-pioneer-program) covers a lot of ground but is often focused on using the emulator and not so much on the details of usign teh cardano-cli ; the [Cardano-node Wiki](https://github.com/input-output-hk/cardano-node-wiki/tree/main/docs) is quite comprehensive and provides a lot of examples but is hard to find and quite unstructured ; the [Official cardano docs](https://docs.cardano.org/introduction/) points to the aforementioned documents
  * :bulb: Having [man pages](https://en.wikipedia.org/wiki/Man_page) or some equivalent inline help system, with details on the various commands, and examples, would be tremendously useful and greatly enhance the developer experience. And it would not be hard to have both textual documentation suitable for use in a console and HTML formatted pages centralised online
* The `transaction build` and `transaction build-raw` commands provide 10s of flags to construct the various parts of a transaction which is extremely unwieldy and error prone
  * :bulb: It would be much easier for clients to provide the various parts using a JSON or YAML file or input
* The output of `query utxo` or `transaction view` is not easily consumed (I had to implement a parser to extract the various parts) and therefore cannot be used to pipe into other commands, eg. `transaction build`
  * :bulb: comm

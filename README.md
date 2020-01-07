# blockchain-etl

[![Build status](https://badge.buildkite.com/a1f6f5135577f0039b6bd6821d5c8f87466ff9c024dbf4a60b.svg)](https://buildkite.com/helium/blockchain-etl)

This is an Erlang application to follow the Helium blockchain and
store it in a Postgres database. This _ingest service_ tracks all
blocks as they're addded to the blockchain by running a full node and
listening for new block events.


## Developer Usage

* Clone this repository
* Create `.env` file by copying `.env.template` and editing it to
  reflect your postgres and other keys and credentials
* Run `make release` in the top level folder
* Run `make reset` to initialize the database and reset the
  ledger. You will need to run a make reset every time the schema or
  code importing the blockchain is changed.

  Running a `make reset` will keep the existing downloaded blocks but
  _replay_ the ledger so the application can re-play the blocks into
  the database
* Run `make start` to start the application. Logs will be at
  `_build/default/rel/blockchain_etl/log/*`.

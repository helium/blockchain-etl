# blockchain-etl

[![Build status](https://badge.buildkite.com/a1f6f5135577f0039b6bd6821d5c8f87466ff9c024dbf4a60b.svg)](https://buildkite.com/helium/blockchain-etl)

This is an Erlang application to follow the Helium blockchain and
store it in a Postgres database. This _ingest service_ tracks all
blocks as they're addded to the blockchain by running a full node and
listening for new block events.


## Dependencies

To run blockchain-etl, you will need:

* erlang 22 -- newer versions will not work
* rust -- we recommend using [rustup](https://rustup.rs/)
* postgresql + postgis

Optionally you can geocode locations using the [Google Maps Geocoding API](https://developers.google.com/maps/documentation/geocoding/start).
Register an API key and update `GOOGLE_MAPS_API_KEY=...` in your `.env.dev` file

## Installing Erlang 22 on Ubuntu

```bash
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update
sudo apt install esl-erlang=1:22.3.4.1-1 cmake libsodium-dev libssl-dev build-essential
```

## Developer Usage

* Clone this repository
* Create `.env` file by copying `.env.template` and editing it to
  reflect your postgres and other keys and credentials

  **Note:** In order for resets to work the postgres user specified in
  the `.env` file needs to exist and have `CREATEDB` permissions.

* Run `make release` in the top level folder
* Run `make reset` to initialize the database and reset the ledger. You will
  need to run a make reset every time the release notes indicate to do so. This
  should be very rare. .

  Running a `make reset` will keep the existing downloaded blocks but _replay_
  the ledger so the application can re-play the blocks into the database. Again,
  only do this when indicated in the release notes since a replay can take a
  long time.
* Run `make start` to start the application. Logs will be at
  `_build/dev/rel/blockchain_etl/log/*`.

Once started the application will start syncing the blockchain and
loading blocks into the attached database.

You can change the release target (and .env file) using the `PROFILE`
environment variable, which defaults to `dev`. i.e.:

```
make start -e PROFILE=testnet
```

#### Note
You may see an error similar to the following during initial sync:

`{error,"IO error: While open a file for appending: data/blockchain.db/020311.sst: Too many open files"}`

Check [this](https://superuser.com/a/443168) Superuser answer for a workaround
on macOS and
[here](https://docs.riak.com/riak/kv/latest/using/performance/open-files-limit/index.html)
for some instructions on various Linux distributions.


## WARNING

Schema changes _will_ happen in this repo as we flesh out the
corresponding APIs. A schema change _may_ require a `make reset` to
reset the database and associated blockchain ledger. On a reset the
blockchain store itself is not affected but the ledger is replayed
which allows the application to reload the database.

**NOTE**: Please refer to the release notes for each release. Unless otherwise
indicated you should _not_ do a `make reset`

## IN PROGRESS

* Docker setup

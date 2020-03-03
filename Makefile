.PHONY: compile rel cover test typecheck doc ci start stop reset

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

shell:
	$(REBAR) shell

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test:
	$(REBAR) as test do eunit,ct

ci:
	$(REBAR) do xref, dialyzer

typecheck:
	$(REBAR) dialyzer

doc:
	$(REBAR) edoc

release:
	$(REBAR) as prod do release

devrelease:
	$(REBAR) as dev do release

start:
	cp -f .env ./_build/prod/rel/blockchain_etl/
	./_build/prod/rel/blockchain_etl/bin/blockchain_etl start

stop:
	-./_build/prod/rel/blockchain_etl/bin/blockchain_etl stop

reset: stop
	cp -f .env ./_build/prod/rel/blockchain_etl/
	rm -rf ./_build/prod/rel/blockchain_etl/data/ledger.db
	rm -rf ./_build/prod/rel/blockchain_etl/log/*
	_build/prod/rel/blockchain_etl/bin/psql_migration reset

resync: stop
	rm -rf ./_build/prod/rel/blockchain_etl/data/ledger.db
	rm -rf ./_build/prod/rel/blockchain_etl/log/*

console:
	./_build/prod/rel/blockchain_etl/bin/blockchain_etl remote_console

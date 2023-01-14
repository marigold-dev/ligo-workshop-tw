.PHONY: build test ligo-version
.PHONY: clean tarball-app
.PHONY: install get-tezos-binary gen_wallet deploy
.PHONY: borrow show_contract

# Environment variables

# The version for artifacts releases
PROJECT_NAME:=$(shell basename $(CURDIR))
BUILD_VERISON := $(shell git describe --always)

# The path of directory
BUILD_DIRECTORY:=_build
APP_DIRECTORY:=app
TEST_DIRECTORY:=test
BUILT_APP_DIRECTORY:=$(BUILD_DIRECTORY)/$(APP_DIRECTORY)

# Ligo compiler
LIGO_COMPILER_ARGS:=--protocol kathmandu
LIGO_VERSION:=0.57.0
LIGO?=ligo
LIGO_BUILD=$(LIGO) compile contract $(LIGO_COMPILER_ARGS)
LIGO_TEST=$(LIGO) run test
LIGO_CURRENT_VERSION:=$(shell $(LIGO) --version)
LIGO_INSTALL=$(LIGO) install

# Tezos binaries
TEZOS_BINARIES_VERSION:=v15.1-1
TEZOS_BINARIES_REPO:=https://github.com/serokell/tezos-packaging/releases/download/
TEZOS_BINARIES_URL:=$(TEZOS_BINARIES_REPO)$(TEZOS_BINARIES_VERSION)

# Toplevel rules
all: build test tarball-app

install:
	ligo install

build: check-ligo-version
	mkdir -p $(BUILT_APP_DIRECTORY)
	$(LIGO_BUILD) $(APP_DIRECTORY)/main.mligo > $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME).tez

test:
	$(LIGO_TEST) $(TEST_DIRECTORY)/test.mligo

clean:
	rm -rf $(BUILD_DIRECTORY)
	rm -rf _esy
	rm -rf .ligo
	rm -rf esy.lock

tarball-app:
	tar czvf $(BUILD_DIRECTORY)/$(PROJECT_NAME)-$(BUILD_VERISON).tar.gz $(BUILT_APP_DIRECTORY)/*.tez

ligo-version:
	@echo $(LIGO_VERSION)

check-ligo-version:
ifneq (${LIGO_VERSION},${LIGO_CURRENT_VERSION})
	$(error Unexpected ligo version (found: ${LIGO_CURRENT_VERSION}, expected: ${LIGO_VERSION}))
endif

gen-wallet:
	@echo -e "\e[32m!!! Please use in ghostnet only !!!\e[0m"
	@$(BUILD_DIRECTORY)/octez-client gen keys wallet_address -f 2> /dev/null
	@$(BUILD_DIRECTORY)/octez-client show address wallet_address -S 2> /dev/null
	@echo -e "\e[32m!!! Please go https://faucet.marigold.dev/ or https://faucet.ghostnet.teztnets.xyz/ to request some XTZ !!!\e[0m"

deploy:
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev originate contract $(PROJECT_NAME) transferring 10 from wallet_address running $(BUILT_APP_DIRECTORY)/$(PROJECT_NAME).tez --init 'Pair (Pair "KT1SkehrJkQaX1emyagMmtoPhpxPXfZ4agDQ" {}) 2' --burn-cap 1 -f

get-tezos-binary:
	wget -O $(BUILD_DIRECTORY)/octez-client $(TEZOS_BINARIES_URL)/octez-client
	chmod +x $(BUILD_DIRECTORY)/octez-client

borrow:
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev transfer 0 from wallet_address to $(PROJECT_NAME) --entrypoint borrow --arg 'Pair 3 "tz1inzFwAjE4oWXMabJFZdPHoDQN5S4XB3wH"' --burn-cap 1


show_contract:
	$(BUILD_DIRECTORY)/octez-client --endpoint https://ghostnet.tezos.marigold.dev list known contracts 2> /dev/null | grep $(PROJECT_NAME)
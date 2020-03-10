.DEFAULT_GOAL := help

clean: ##@building Clean build data
	dune clean

build: ##@building Build all executables
	dune build

rebuild: ##@building Build with enhanced context output (requires Refmterr)
	refmterr dune build

run: build ##@running Run main executable
	./_build/install/default/bin/sbg run

debug: build ##@running Run (debug) main executable
	LD_LIBRARY_PATH=/usr/local/lib rlwrap ocamldebug ./_build/default/bin/main.bc

runtests: build ##@@running Run tests in test/ directory
	./_build/install/default/bin/RunTests

GREEN  := $(shell tput -Txterm setaf 2)
WHITE  := $(shell tput -Txterm setaf 7)
YELLOW := $(shell tput -Txterm setaf 3)
RESET  := $(shell tput -Txterm sgr0)
HELP_FUN = \
    %help; while(<>) { push @{$$help{$$2 // 'options'}}, [$$1, $$3] if /^([a-zA-Z\-]+)\s*:.*\#\#(?:@([a-zA-Z\-]+))?\s(.*)$$/ }; \
    print "Usage: make [target]\n\n"; for (sort keys %help) { print "${WHITE}$$_:${RESET}\n"; for (@{$$help{$$_}}) { \
    $$sep = " " x (32 - length $$_->[0]); print "  ${YELLOW}$$_->[0]${RESET}$$sep${GREEN}$$_->[1]${RESET}\n"; }; print "\n"; }

help: ##@other Show this help.
	@perl -e '$(HELP_FUN)' $(MAKEFILE_LIST)

.PHONY: help clean build rebuild link_stubs curses_howto lymp_howto prepare prepare_curses prepare_lymp run runtests

REBAR = $(shell pwd)/rebar3

.PHONY: all deps compile rel run prod

all: deps compile rel

deps:
	$(REBAR) deps

compile:
	$(REBAR) compile

rel:
	$(REBAR) release

run:
	$(shell pwd)/_build/default/rel/mcserv/bin/mcserv console

rel:
	$(REBAR) release

prod:
	$(REBAR) as prod tar

REBAR = $(shell pwd)/rebar3

.PHONY: server client run

all: server client

server:
	$(REBAR) as server release

client:
	$(REBAR) as client escriptize

run:
	$(shell pwd)/_build/server/rel/mcserv/bin/mcserv console

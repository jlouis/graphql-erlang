REBAR ?= rebar3
PROJECT = graphql
VERSION = 0.0.1

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

test:
	@$(REBAR) ct --spec test/test.spec

test-cover:
	@$(REBAR) do ct -c, cover -v

update:
	@$(REBAR) update

.PHONY: compile clean dialyzer test update

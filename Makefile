REBAR   = rebar3
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
	@$(REBAR) ct

test-cover:
	@$(REBAR) ct --cover

update:
	@$(REBAR) update

.PHONY: compile clean dialyzer test update

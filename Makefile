REBAR   = rebar3
PROJECT = gql
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

update:
	@$(REBAR) update

.PHONY: compile clean dialyzer test update

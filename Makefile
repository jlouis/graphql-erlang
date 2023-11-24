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
	@$(REBAR) ct --verbosity 60 --spec test/test.spec

test-cover:
	@$(REBAR) do ct -c --spec test/test.spec, cover -v

xref:
	@$(REBAR) xref

update:
	@$(REBAR) update

doc:
	@$(REBAR) edoc

.PHONY: compile clean dialyzer test update

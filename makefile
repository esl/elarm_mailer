.PHONY: clean compile test test-deps

REBAR=rebar
REBAR_TEST=$(REBAR) -C rebar.test.config

test:	test-deps compile
	$(REBAR_TEST) ct

test-deps:
	$(REBAR_TEST) get-deps

compile:
	$(REBAR_TEST) compile

clean:
	$(REBAR_TEST) clean

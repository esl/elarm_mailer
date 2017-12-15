.PHONY: clean compile devrun test test-deps

REBAR=rebar
REBAR_TEST=$(REBAR) -C rebar.test.config

test:	test-deps compile
	$(REBAR) ct skip_deps=true

test-deps:
	$(REBAR_TEST) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

devsmtp:
	python -m smtpd -n -c DebuggingServer localhost:2525

devrun: compile
	cp ./priv/elarm_mailer.app.dev ./ebin/elarm_mailer.app
	erl -pa ./deps/*/ebin -pa ebin -s elarm_mailer start

shell: compile
	cp ./priv/elarm_mailer.app.dev ./ebin/elarm_mailer.app
	erl -pa ./deps/*/ebin -pa ebin

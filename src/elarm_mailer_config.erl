-module(elarm_mailer_config).
-export([get_env/1]).

get_env(Key) ->
    case application:get_env(elarm_mailer, Key) of
        {ok, Val} -> Val;
        undefined -> default_value(Key)
    end.

default_value(elarm_server) -> elarm_server;
default_value(gen_smtp_options) -> [];
default_value(recipients) -> [];
default_value(sender) -> "nobody@nohost";
default_value(subscribed_alarms) -> [];
default_value(formatter) -> none;
default_value(X) -> error({missing_default_value, X}).

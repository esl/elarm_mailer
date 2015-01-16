-module(elarm_mailer_config).
-export([get_env/1]).


get_env(Key) ->
    case application:get_env(elarm_mailer, Key) of
        {ok, Val} -> Val;
        undefined -> default_value(Key)
    end.

default_value(sender) -> "nobody@nohost";
default_value(recipients) -> [];
default_value(gen_smtp_options) -> [];
default_value(subscribed_atoms) -> [];
default_value(X) -> error({missing_default_value, X}).



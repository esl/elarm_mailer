-module(elarm_mailer_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([subscribe_from_config/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = elarm_mailer_sup:start_link(),
    subscribe_from_config(),
    {ok, Sup}.

subscribe_from_config() ->
    Env = fun(Key) -> elarm_mailer_config:get_env(Key) end,
    ok = elarm_mailer:subscribe_to_alarms(Env(sender),
        Env(recipients),
        Env(gen_smtp_options),
        Env(elarm_server),
        Env(subscribed_alarms)).

stop(_State) ->
    ok.


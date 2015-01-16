-module(elarm_mailer_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Env = fun(Key) -> elarm_mailer_config:get_env(Key) end,
    {ok, Sup} = elarm_mailer_sup:start_link(),
    ok = elarm_mailer:subscribe_to_alarms
           (Env(sender),
            Env(recipients),
            Env(gen_smtp_options),
            Env(subscribed_alarms)),
    {ok, Sup}.

stop(_State) ->
    ok.


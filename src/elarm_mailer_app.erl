-module(elarm_mailer_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% todo specify temp dir in config
    os:cmd("mkdir -p /tmp/elarm_mailer"),
    {ok, Sup} = elarm_mailer_sup:start_link(),
    ok = elarm_mailer:subscribe_to_alarms(app_config_header(sender),
                                          app_config_header(recipients),
                                          app_config_header(gen_smtp_options),
                                          app_config_alarms()),
    {ok, Sup}.

stop(_State) ->
    ok.

app_config_header(Key) ->
    {ok, Val} = application:get_env(elarm_mailer, Key),
    Val.

app_config_alarms() ->
    case application:get_env(elarm_mailer, subscribed_alarms) of
        undefined -> [];
        {ok, Alarms} -> Alarms
    end.

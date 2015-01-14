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
    ok = elarm_mailer:subscribe_to_alarms(app_config_header(from),
                                          app_config_header(to),
                                          app_config_alarms()),
    {ok, Sup}.

stop(_State) ->
    ok.

app_config_header(Atom) ->
    {ok, Plist} = application:get_env(elarm_mailer, sendmail_headers),
    {Atom, Val} = lists:keyfind(Atom, 1, Plist),
    Val.

app_config_alarms() ->
    case application:get_env(elarm_mailer, subscribed_alarms) of
        undefined -> [];
        {ok, Alarms} -> Alarms
    end.

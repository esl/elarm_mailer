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
    elarm_mailer_sup:start_link().

stop(_State) ->
    ok.

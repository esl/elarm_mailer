-module(elarm_mailer).
-export([subscribe_to_alarm/3, subscribe_to_alarms/3]).
-export([get_subscribed_alarms/0]).
-export([start/0]).

-define(WORKER_SUP, elarm_mailer_sup).

start() ->
    %% For local testing
    application:start(elarm),
    ok = application:start(elarm_mailer).

get_subscribed_alarms() ->
    [ {elarm_mailer_worker:get_alarm(Worker), Worker}
      || {_,Worker,_,_} <- supervisor:which_children(?WORKER_SUP) ].

subscribe_to_alarm(From, To, AlarmName) ->
    {ok, _P} = supervisor:start_child(?WORKER_SUP, [From, To, AlarmName]).

subscribe_to_alarms(From, To, Alarms) ->
    lists:foreach(fun(A) -> subscribe_to_alarm(From, To, A) end, Alarms),
    ok.

-module(elarm_mailer).
-export([subscribe_to_alarm/4, subscribe_to_alarms/4]).
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

subscribe_to_alarm(From, To, GenSmtpOptions, AlarmName) ->
    {ok, _P} = supervisor:start_child
                 (?WORKER_SUP, [From, To, GenSmtpOptions, AlarmName]).

subscribe_to_alarms(From, To, GenSmtpOptions, Alarms) ->
    [ subscribe_to_alarm(From, To, GenSmtpOptions, A) || A <-  Alarms ],
    ok.

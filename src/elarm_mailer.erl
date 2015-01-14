-module(elarm_mailer).
-export([subscribe_all/3]).
-export([subscribe_to/3]).
-define(WORKER_SUP, elarm_mailer_sup).

subscribe_all(From, To, Alarms) ->
    lists:foreach(fun(Alarm) -> subscribe_to(From, To, Alarm) end,
                  Alarms),
    ok.

subscribe_to(From, To, AlarmName) ->
    {ok, _Pid} = supervisor:start_child(elarm_mailer_sup, [From, To, AlarmName]).

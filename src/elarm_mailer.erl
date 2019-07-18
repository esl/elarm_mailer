-module(elarm_mailer).
-export([subscribe_to_alarm/6, subscribe_to_alarms/6, subscribe_to_alarm/5, subscribe_to_alarms/5]).
-export([get_subscribed_alarms/0]).
-export([start/0]).
-export([reload_config/0]).

-define(WORKER_SUP, elarm_mailer_sup).

start() ->
    %% For local testing
    application:start(elarm),
    ok = application:start(elarm_mailer).

get_subscribed_alarms() ->
    [ {elarm_mailer_worker:get_alarm(Worker), Worker}
      || {_,Worker,_,_} <- supervisor:which_children(?WORKER_SUP) ].

subscribe_to_alarm(From, To, GenSmtpOptions, ElarmServer, AlarmName) ->
  subscribe_to_alarm(From, To, GenSmtpOptions, ElarmServer, AlarmName, elarm_mailer_email).

subscribe_to_alarm(From, To, GenSmtpOptions, ElarmServer, AlarmName, FormatterModule) ->
    {ok, _P} =
        supervisor:start_child(?WORKER_SUP, [From, To,
                                             GenSmtpOptions,
                                             ElarmServer, AlarmName, FormatterModule]).

subscribe_to_alarms(From, To, GenSmtpOptions, ElarmServer, Alarms) ->
  [ subscribe_to_alarm(From, To, GenSmtpOptions, ElarmServer, A, elarm_mailer_email)
    || A <-  Alarms ],
  ok.

subscribe_to_alarms(From, To, GenSmtpOptions, ElarmServer, Alarms, FormatterModule) ->
    [ subscribe_to_alarm(From, To, GenSmtpOptions, ElarmServer, A, FormatterModule)
      || A <-  Alarms ],
    ok.


reload_config() ->
    SubscriberPids = [
        Pid || {_, Pid, _, _} <- supervisor:which_children(elarm_mailer_sup)],
    [supervisor:terminate_child(elarm_mailer_sup, Pid) || Pid <- SubscriberPids],
    elarm_mailer_app:subscribe_from_config().
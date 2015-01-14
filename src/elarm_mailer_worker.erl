-module(elarm_mailer_worker).
-behavior(gen_server).
-export([start_link/3]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-include_lib("elarm/include/elarm.hrl").
-record(state, { from :: string(),
                 to :: string(),
                 subscribed_alarm :: atom() }).

start_link(From, To, AlarmName) ->
    gen_server:start_link(?MODULE, [From, To, AlarmName], []).

init([From, To, AlarmName]) ->
    error_logger:error_msg("worker started with args: ~p", [{From, To, AlarmName}]),
    {_Ref, _, _} = elarm:subscribe([all], self()),
    {ok, #state{ from = From, to = To, subscribed_alarm = AlarmName }}.

terminate(_,_) -> ok.
handle_call(_,_,S) -> {reply, ok, S}.
handle_cast(_,S) -> {noreply, S}.

handle_info({elarm, _, #alarm{alarm_id=AlarmName} = A},
            #state{subscribed_alarm=AlarmName} = S) ->
    elarm_mailer_sendmail:send(S#state.from, S#state.to, A),
    {noreply, S};
handle_info(_, S) -> {noreply, S}.

code_change(_,_,_) ->
    ok.

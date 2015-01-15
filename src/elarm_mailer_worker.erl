-module(elarm_mailer_worker).
-behavior(gen_server).

-export([get_alarm/1]).
-export([start_link/4]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3]).

-include_lib("elarm/include/elarm.hrl").
-record(state, { from :: string(),
                 to :: string(),
                 subscribed_alarm :: atom(),
                 gen_smtp_options :: list() %% see gen_smtp_client.erl
               }).

start_link(From, To, GenSmtpOptions, AlarmName) ->
    gen_server:start_link(?MODULE, [From, To, GenSmtpOptions, AlarmName], []).

get_alarm(Worker) ->
    gen_server:call(Worker, get_alarm).

init([From, To, GenSmtpOptions, AlarmName]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("worker started with args: ~p\n", [{From, To, GenSmtpOptions, AlarmName}]),
    {_Ref, _, _} = elarm:subscribe([all], self()),
    {ok, #state{ from = From, to = To,
                 subscribed_alarm = AlarmName, gen_smtp_options = GenSmtpOptions }}.

terminate(_R,_S) ->
    %% error_logger:error_msg("elarm mailer worker ~p terminating ~p, ~p~n", [self(), R, S]),
    ok.

handle_call(get_alarm,_,S) -> {reply, S#state.subscribed_alarm, S};
handle_call(_,_,S) -> {reply, ok, S}.

handle_cast(_,S) -> {noreply, S}.

handle_info({elarm, _, #alarm{alarm_id=AlarmName} = A},
            #state{subscribed_alarm=AlarmName} = S) ->
    elarm_mailer_email:send(S#state.from, S#state.to, S#state.gen_smtp_options, A),
    {noreply, S};
handle_info(_, S) -> {noreply, S}.

code_change(_,_,_) ->
    ok.

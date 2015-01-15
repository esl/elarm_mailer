-module(elarm_mailer_email).
-export([make_body/3]).
-include_lib("elarm/include/elarm.hrl").

make_body(Sender, Recipients, Alarm) ->
    flatten(Sender, hd(Recipients), subject(Alarm), format(Alarm)).

%% TODO: Consider something smart, like a templating engine.

format(Term) ->
    Nice = list_to_binary(lists:flatten(io_lib:format("~p", [Term]))),
    Flat = binary:replace(Nice, <<"\n">>, <<>>, [global]),
    NoSpaces = binary:replace(Flat, <<"  ">>, <<>>, [global]),
    binary_to_list(NoSpaces).

flatten(From, To, Subject, MessageText) ->
    lists:flatten(["Subject: ", Subject , "\r\n", "From: ", From, "\r\n",
                   "To: ", To, "\r\n", "\r\n", MessageText]).

subject(#alarm{alarm_id = AlarmId, src = AlarmSrc}) ->
    io_lib:format("Alarm! ~p : ~p", [AlarmId, AlarmSrc]).

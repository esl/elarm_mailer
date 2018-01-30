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
    % Using 1000p in order to avoid linebreaks for long alarm messages
    % Linebreaks in the email message violate format of email messages
    % And are causing gen_smtp failures
    % Please also refer related RFC http://www.faqs.org/rfcs/rfc2822.html
    io_lib:format("Alarm! ~1000p : ~1000p", [AlarmId, AlarmSrc]).

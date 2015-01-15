-module(elarm_mailer_email).
-export([make_body/3]).

make_body(Sender, Recipients, Alarm) ->
    flatten(Sender, hd(Recipients), format(Alarm)).

format(Term) ->
    Nice = list_to_binary(lists:flatten(io_lib:format("~p", [Term]))),
    Bin = binary:replace(Nice, <<"\n">>, <<>>, [global]),
    binary_to_list(Bin).

flatten(From, To, MessageText) ->
    lists:flatten(["Subject: Alarm\r\n", "From: ", From, "\r\n",
                   "To: ", To, "\r\n", "\r\n", MessageText]).

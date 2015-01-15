-module(elarm_mailer_email).
-export([send/4]).

send(From, To, GenSmtpOptions, Alarm) ->
    gen_smtp_client:send({From, To, format_email(From, hd(To), format(Alarm))},
                         GenSmtpOptions).

format(Term) ->
    Nice = list_to_binary(lists:flatten(io_lib:format("~p", [Term]))),
    Bin = binary:replace(Nice, <<"\n">>, <<>>, [global]),
    binary_to_list(Bin).

format_email(From, To, MessageText) ->
    lists:flatten(["Subject: Alarm\r\n", "From: ", From, "\r\n",
                   "To: ", To, "\r\n", "\r\n", MessageText]).

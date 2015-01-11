-module(elarm_mailer_sendmail).
-export([send/1]).

send(Alarm) ->
    {ok, Cmd} = application:get_env(elarm_mailer, sendmail_command),
    os:cmd(Cmd ++ " " ++ format(Alarm)).

format(Term) ->
    %% todo: shell-escaping
    Nice = list_to_binary(lists:flatten(io_lib:format("'~p'", [Term]))),
    Bin = binary:replace(Nice, <<"\n">>, <<>>, [global]),
    binary_to_list(Bin).

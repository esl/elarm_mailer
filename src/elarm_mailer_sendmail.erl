-module(elarm_mailer_sendmail).
-export([send/3]).

send(From, To, Alarm) ->
    {ok, Cmd} = application:get_env(elarm_mailer, sendmail_command),
    TempFileName = ref_to_filename(make_ref()),
    {ok, TempFile} = file:open(TempFileName, [write]),
    file:write(TempFile, format_email(From, To, format(Alarm))),
    _CmdRes = os:cmd(Cmd ++ " -vt < " ++ TempFileName).
    %% error_logger:format("cmd result is ~p", [CmdRes]),
    %% error_logger:format("/tmp is ~s", [os:cmd("ls /tmp")]).

format(Term) ->
    %% todo: shell-escaping
    Nice = list_to_binary(lists:flatten(io_lib:format("~p", [Term]))),
    Bin = binary:replace(Nice, <<"\n">>, <<>>, [global]),
    binary_to_list(Bin).

format_email(From, To, MessageText) ->
    lists:flatten(["Subject: Alarm\n",
                   "From: ", From, "\n",
                   "To: ", To, "\n",
                   "\n\n",
                   MessageText]).

ref_to_filename(Ref) ->
    B = list_to_binary(lists:flatten(io_lib:format("~p", [Ref]))),
    Clean = lists:foldl(fun(ToErase, Subject) -> binary:replace(Subject, ToErase, <<>>, [global]) end,
                        B,
                        [<<"#">>, <<"<">>, <<">">>]),
    "/tmp/elarm_mailer/" ++ binary_to_list(Clean).

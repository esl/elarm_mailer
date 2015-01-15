-module(elarm_mailer_test_mailbox).

-export([start/0, stop/0]).
-export([clear/0, dump/0]).
-export([loop/1]).

-define(PROC_NAME, ?MODULE).

start() ->
    P = spawn_link(fun() -> loop([]) end),
    register(?PROC_NAME, P),
    ok.

stop() -> call(stop).

clear() -> call(clear_email_list).

dump() -> call(dump_email_list).

loop(Emails) ->
    receive
        {stop, From} -> From ! {ok, stopped};
        {clear_email_list, From} -> From ! ok, loop([]);
        {dump_email_list, From} -> From ! lists:reverse(Emails), loop(Emails);
        Email -> loop([Email|Emails])
    end.

call(Cmd) ->
    ?PROC_NAME ! {Cmd, self()},
    receive Any -> Any
    after 500 -> error(no_reply_from_mailbox)
    end.

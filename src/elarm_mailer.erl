-module(elarm_mailer).
-export([subscribe_to/1]).
-export([mailer/3]).
-include_lib("elarm/include/elarm.hrl").

subscribe_to(AlarmName) ->
    From = header(from),
    To = header(to),
    MailerF = spawn_link(fun()-> ?MODULE:mailer(From, To, AlarmName) end),
    {Ref, _, _} = elarm:subscribe([all], MailerF),
    {ok, Ref}.

mailer(From, To, AlarmName) ->
    receive
        {elarm, _, #alarm{alarm_id=AlarmName} = A} ->
            elarm_mailer_sendmail:send(From, To, A),
            mailer(From, To, AlarmName);
        _ ->
            mailer(From, To, AlarmName)
    end.

header(Atom) ->
    {ok, Plist} = application:get_env(elarm_mailer, sendmail_headers),
    {Atom, Val} = lists:keyfind(Atom, 1, Plist),
    Val.

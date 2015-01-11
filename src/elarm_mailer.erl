-module(elarm_mailer).
-export([subscribe_to/1]).

subscribe_to(_AlarmName) ->
    MailerF = spawn(fun mailer/0),
    {Ref, _, _} = elarm:subscribe([all], MailerF),
    {ok, Ref}.

mailer() ->
    receive {elarm, _, Alarm} ->
            elarm_mailer_sendmail:send(Alarm),
            mailer();
            _ -> mailer()
    end.


-module(elarm_mailer).
-export([subscribe_to/1]).
-export([mailer/1]).
-include_lib("elarm/include/elarm.hrl").

subscribe_to(AlarmName) ->
    MailerF = spawn(fun()-> ?MODULE:mailer(AlarmName) end),
    {Ref, _, _} = elarm:subscribe([all], MailerF),
    {ok, Ref}.

mailer(AlarmName) ->
    receive
        {elarm, _, #alarm{alarm_id=AlarmName} = A} ->
            elarm_mailer_sendmail:send(A),
            mailer(AlarmName);
        _ ->
            mailer(AlarmName)
    end.

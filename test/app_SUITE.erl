-module(app_SUITE).
-compile([export_all]).
-define(then, fun).

all() ->
    [it_starts_up,
     it_has_access_to_environemt_vars,
     it_sends_mail_on_subscribed_alarm,
     it_doesnt_send_mail_on_unsubscribed_alarm
    ].

it_starts_up(_) ->
    bddr:test(given_nothing,
              when_app_starts(),
              ?then(AppStarted)->  ok = AppStarted end,
              teardown()).

it_has_access_to_environemt_vars(_) ->
    bddr:test(given_app_started(),
              when_app_asks_for_keys(elarm_mailer),
              ?then(Keys) -> {ok, [_|_]} = Keys end,
              teardown()).

it_sends_mail_on_subscribed_alarm(CT) ->
    bddr:test([given_app_started(),
               given_configured_command(mocked_sendmail(CT)),
               given_configured_alarm(peace_attack)],
              when_alarm_gets_raised(peace_attack, "Hello"),
              ?then(_) ->
                     {alarm,peace_attack,undefined,"Hello",_Date,_Time,
                      _,_,_,_,[],[],[],_,_,new, _}
                         = received_email() end,
              teardown()).

it_doesnt_send_mail_on_unsubscribed_alarm(CT) ->
    bddr:test([given_app_started(),
               given_configured_command(mocked_sendmail(CT)),
               given_configured_alarm(peace_attack)],
              when_alarm_gets_raised(pigeons_ahoy, "Plop"),
              ?then(_) -> none = received_email() end,
              teardown()).

given_app_started() -> start_app().

given_configured_alarm(Alarm) ->
    elarm_mailer:subscribe_to(Alarm).

given_configured_command(CmdLocation) ->
    application:set_env(elarm_mailer, sendmail_command, CmdLocation).


when_alarm_gets_raised(Alarm, Entity) ->
    fun(_G) -> elarm:raise(Alarm, Entity, []) end.

when_app_asks_for_keys(AppName) ->
    fun(_G) -> application:get_all_key(AppName) end.

when_app_starts() -> fun(_G) -> start_app() end.

received_email() ->
    timer:sleep(1000),
    case file:read_file(mocked_sent_mail_location()) of
        {ok, B} -> string_to_term(binary_to_list(B));
        {error, enoent} -> none
    end.

%% Plumbing

start_app() ->
    remove_mocked_sent_file(),
    error_logger:tty(false),
    application:start(elarm),
    application:start(elarm_mailer).

teardown() ->
    fun(_Givens) ->
            application:stop(elarm_mailer),
            application:stop(elarm),
            error_logger:tty(true)
    end.

mocked_sendmail(CTConfig) ->
    {data_dir, D} = lists:keyfind(data_dir, 1, CTConfig),
    D ++ "mocked_sendmail".

mocked_sent_mail_location() ->
    %% This currently hard-coded in
    %% test/app_SUITE_data/mocked_sendmail
    "/tmp/mocked_sendmail.out".

remove_mocked_sent_file() ->
    os:cmd("rm -f " ++ mocked_sent_mail_location()).

string_to_term(String) ->
    {ok,Tokens,_} = erl_scan:string(String ++ "."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

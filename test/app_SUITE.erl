-module(app_SUITE).
-compile([export_all]).

-define(given, fun).
-define(_when, fun).
-define(_then, fun).

all() ->
    [it_starts_up,
     it_has_access_to_environemt_vars,
     it_sends_mail_on_subscribed_alarm,
     it_doesnt_send_mail_on_non_subscribed_alarm,
     it_can_report_subscribed_alarms
    ].

it_starts_up(_) ->
    bddr:test(?given() -> no_previous_state end,
              ?_when(_) -> app_is_started() end,
              ?_then(AppStarted) -> ok = AppStarted end,
              teardown()).

it_has_access_to_environemt_vars(_) ->
    bddr:test(?given() -> app_is_started() end,
              ?_when(_) -> app_asks_for_keys(elarm_mailer) end,
              ?_then(Keys) -> {ok, [_|_]} = Keys end,
              teardown()).

it_sends_mail_on_subscribed_alarm(CT) ->
    bddr:test(?given() ->
                     User = local_user(),
                     app_is_started(),
                     configured_command(mocked_sendmail_cmd(CT)),
                     configured_email_headers([{from, User}, {to, User}]),
                     configured_alarms([peace_attack]),
                     alarm_gets_raised(peace_attack, "Hello"),
                     User end,

              ?_when(User) -> user_checks_email(User) end,

              ?_then(Email) ->
                     {alarm,peace_attack,undefined,"Hello",_Date,_Time,
                      _,_,_,_,[],[],[],_,_,new, _}
                         = Email end,
              teardown()).

it_doesnt_send_mail_on_non_subscribed_alarm(CT) ->
    bddr:test(?given()->
                     User = local_user(),
                     app_is_started(),
                     configured_command(mocked_sendmail_cmd(CT)),
                     configured_email_headers([{from, User}, {to, User}]),
                     configured_alarms([peace_attack]),
                     alarm_gets_raised(pigeons_ahoy, "Plop"),
                     User end,

              ?_when(User) -> user_checks_email(User) end,

              ?_then(Email) -> none = Email end,
              teardown()).

it_can_report_subscribed_alarms(CT) ->
    bddr:test(?given()->
                     User = local_user(),
                     app_is_started(),
                     configured_command(mocked_sendmail_cmd(CT)),
                     configured_email_headers([{from, User}, {to, User}]),
                     configured_alarms([george, john, paul, ringo]) end,

              ?_when(_) -> api_is_asked_for_alarms() end,

              ?_then(Alarms) -> [{george,_}, {john,_}, {paul,_}, {ringo,_}] =
                                    lists:sort(Alarms) end,
              teardown()).

given_app_started() -> start_app().

local_user() ->
    string:strip(os:cmd("echo $USER"), right, $\n) ++ "@localhost".

subscribed_alarm(Alarm) ->
    elarm_mailer:subscribe_to(Alarm).

configured_command(CmdLocation) ->
    application:set_env(elarm_mailer, sendmail_command, CmdLocation).

configured_email_headers(Headers) ->
    application:set_env(elarm_mailer, sendmail_headers, Headers).

configured_alarms(Alarms) when is_list(Alarms) ->
    application:set_env(elarm_mailer, subscribed_alarms, Alarms),
    %% we do this so the application can read its env again, with the new alarms
    %% It will subscribe to them upon starting
    application:stop(elarm_mailer),
    application:start(elarm_mailer).

alarm_gets_raised(Alarm, Entity) ->
    elarm:raise(Alarm, Entity, []).

app_asks_for_keys(AppName) ->
    application:get_all_key(AppName).

app_is_started() -> start_app().

api_is_asked_for_alarms() ->
    elarm_mailer:get_subscribed_alarms().

user_checks_email(Username) ->
    timer:sleep(300),
    Loc = mocked_sent_mail_location(Username, Username),
    case file:read_file(Loc) of
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

mocked_sendmail_cmd(CTConfig) ->
    {data_dir, D} = lists:keyfind(data_dir, 1, CTConfig),
    D ++ "mocked_sendmail".

mocked_sent_mail_location(From, To) ->
    %% This currently hard-coded in
    %% test/app_SUITE_data/mocked_sendmail
    "/tmp/from." ++ From ++ ".to." ++ To ++ ".mocked_sendmail.out".

remove_mocked_sent_file() ->
    os:cmd("rm -f " ++ mocked_sent_mail_location("*", "*")).

string_to_term(String) ->
    {ok,Tokens,_} = erl_scan:string(String ++ "."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

-module(app_SUITE).
-include_lib("elarm/include/elarm.hrl").
-compile([export_all]).

-define(given, fun).
-define(_when, fun).
-define(_then, fun).

-define(SMTP_HOST, "localhost").
-define(SMTP_PORT, 2525).

all() -> [{group, main}].

groups() ->
    [{main, [shuffle, {repeat, 5}],
      [it_starts_up,
       it_sends_mail_on_subscribed_alarm,
       it_doesnt_send_mail_on_non_subscribed_alarm,
       it_can_report_subscribed_alarms,
       when_restarted_resubscribes_to_alarms
      ]}
    ].

it_starts_up(_) ->
    bddr:test(?given() ->
                     smtp_server_running(),
                     local_user() end,
              ?_when(User) ->
                     app_is_started_with_config
                       (options_for(User, "admin@example.org")
                        ++ [{subscribed_alarms, []}]) end,
              ?_then(AppStarted) ->
                     ok = AppStarted end,
              teardown()).

it_sends_mail_on_subscribed_alarm(CT) ->
    bddr:test(?given() ->
                     User = local_user(),
                     smtp_server_running(),
                     app_is_started_with_config
                       (options_for(User, "foo@example.com")
                        ++ [{subscribed_alarms, [peace_attack]}]),
                     alarm_gets_raised(peace_attack, "Hello") end,

              ?_when(_) -> user_checks_email("foo@example.com") end,

              ?_then(Email) ->
                     {{from, User},
                      {to, ["foo@example.com"]},
                      {body, {peace_attack, "Hello"}}}
                         = readable(hd(Email)) end,
              teardown()).

it_doesnt_send_mail_on_non_subscribed_alarm(CT) ->
    bddr:test(?given()->
                     User = local_user(),
                     smtp_server_running(),
                     app_is_started_with_config
                       (options_for(User, "bar@example.com")
                        ++ [{subscribed_alarms, [peace_attack]}]),
                     alarm_gets_raised(pigeon_attack, "Plop") end,

              ?_when(_) -> user_checks_email("bar@example.com") end,

              ?_then(Email) -> [] = Email end,

              teardown()).

it_can_report_subscribed_alarms(CT) ->
    bddr:test(?given()->
                     User = local_user(),
                     smtp_server_running(),
                     app_is_started_with_config
                       (options_for(User, "baz@example.com") ++
                            [{subscribed_alarms, [george, john, paul, ringo]}]) end,

              ?_when(_) -> api_is_asked_for_alarms() end,

              ?_then(Alarms) ->
                     [{george,_}, {john,_}, {paul,_}, {ringo,_}] =
                         lists:sort(Alarms) end,
              teardown()).

when_restarted_resubscribes_to_alarms(CT) ->
    bddr:test(?given()->
                     User = local_user(),
                     smtp_server_running(),
                     app_is_started_with_config(
                        options_for(User, "baz@example.com") ++
                        [{subscribed_alarms, [attack]}])
              end,
              ?_when(_) ->
                env_config_sender_changed_to("foo@example.com"),
                elarm_mailer_config_is_reloaded(),
                alarm_gets_raised(attack, "Plop")
              end,
              ?_then(_) ->
                assert_all_emails_are_from(<<"foo@example.com">>)
              end,
teardown()).

local_user() ->
    string:strip(os:cmd("echo $USER"), right, $\n) ++ "@localhost".

alarm_gets_raised(Alarm, Entity) ->
    elarm:raise(Alarm, Entity, []),
    timer:sleep(300).

app_is_started_with_config(Config) -> start_app(Config).

api_is_asked_for_alarms() ->
    elarm_mailer:get_subscribed_alarms().

user_checks_email(Username) ->
    ct:pal("Messages in box: ~p",[elarm_mailer_test_mailbox:dump()]),
    message_with_recipeint(list_to_binary(Username),
                           elarm_mailer_test_mailbox:dump()).

message_with_recipeint(RecipientName, Mailbox) ->
    case lists:keyfind({to, [RecipientName]}, 3, Mailbox) of
        false -> [];
        Val -> [Val] end.

readable({_, {from, F}, {to, T}, {body, B}}) ->
    {{from, binary_to_list(F)},
     {to, lists:map(fun binary_to_list/1, T)},
     {body, extract_terms(B)}}.

extract_terms(Body) ->
    %% error_logger:error_msg("body~p~n",[Body]),
    [_Before, After] = binary:split(Body, list_to_binary("\r\n\r\n")),
    A = parse_binary(After),
    {A#alarm.alarm_id, A#alarm.src}.

smtp_server_running() ->
    elarm_mailer_test_mailbox:start(),
    {ok, _Pid} = gen_smtp_server:start_link(elarm_mailer_test_smtp_server),
    timer:sleep(300).

%% Plumbing
start_app([]) ->
    application:start(elarm),
    ok = application:start(elarm_mailer);
start_app(Config) ->
    Get = fun(Key) -> element(2, lists:keyfind(Key, 1, Config)) end,
    Swap = fun(Key) -> application:set_env(elarm_mailer, Key, Get(Key)) end,
    error_logger:tty(false),
    application:start(elarm),
    application:load(elarm_mailer),
    [ Swap(Key) ||
        Key <- [sender, recipients, gen_smtp_options, subscribed_alarms] ],
    ok = application:start(elarm_mailer).

teardown() ->
    fun(_Givens) ->
            elarm_mailer_test_mailbox:stop(),
            application:stop(elarm_mailer),
            ok = application:stop(elarm),
            error_logger:tty(true)
    end.

parse_binary(Bin) ->
    String = binary_to_list(Bin),
    {ok,Tokens,_} = erl_scan:string(String ++ "."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.

options_for(User, Recipient) ->
    [{sender, User},
     {recipients, [Recipient]},
     {gen_smtp_options, [{relay, ?SMTP_HOST},
                         {username, User},
                         {password, ""},
                         {port, ?SMTP_PORT}]}].

env_config_sender_changed_to(Sender) ->
    application:set_env(elarm_mailer, sender, Sender).

elarm_mailer_config_is_reloaded() ->
    elarm_mailer:reload_config().

get_received_emails() ->
    elarm_mailer_test_mailbox:dump().

assert_all_emails_are_from(ExpectedFrom) ->
    lists:foreach(fun(Email) ->
                    {_Ts, {from, From}, {to, To}, _} = Email,
                    From = ExpectedFrom
                  end, get_received_emails()).
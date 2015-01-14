-module(elarm_mailer_sup).
-behaviour(supervisor).
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    WorkerSpec = {elarm_mailer_worker,
                  {elarm_mailer_worker, start_link, []},
                  temporary, 2000, worker, [elarm_mailer_worker]},
    {ok, { {simple_one_for_one, 5, 10}, [WorkerSpec]}}.

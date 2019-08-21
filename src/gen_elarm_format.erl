%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Erlang Solutions Ltd
%%% @doc
%%% Behaviour module for elarm email formatting.
%%% @end
%%%-------------------------------------------------------------------
-module(gen_elarm_format).

-include_lib("elarm/include/elarm.hrl").

-callback make_body(From :: string(), To :: string(), Alarm :: #alarm{}) -> term().

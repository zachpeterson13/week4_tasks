%%%-------------------------------------------------------------------
%% @doc week4_tasks public API
%% @end
%%%-------------------------------------------------------------------

-module(week4_tasks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    week4_tasks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

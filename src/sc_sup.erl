-module(sc_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/2]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

init(_) ->
  Element = {sc_elelment, {sc_elelment, start_link, []},
            temporary, brutal_kill, worker, [sc_elelment]}, 
  Children = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.
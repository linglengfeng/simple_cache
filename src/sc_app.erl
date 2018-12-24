-module(sc_app).
-behaviour(application).
-export([start/2, 
        stop/1, 
        ensure_contact/0,
        ensure_contact/1,
        wait_for_nodes/2,
        wait_for_nodes/3,
        get_env/3]).

start(_StarType, _StarArgs) ->
  ok = ensure_contact(),
  sc_store:init(),
  case sc_sup:start_link() of
    {ok, Pid} ->
      {ok, Pid};
    Other ->
      {error, Other}
  end.

stop(_State) ->
  ok.

ensure_contact() ->
  DefaultNodes = ['contact1@ZWWXD164', 'contact2@ZWWXD164'],
  case get_env(simple_cache, contact_nodes, DefaultNodes) of
    [] ->
      {error, no_contact_nodes};

    ContactNodes ->
      ensure_contact(ContactNodes)
  end.

ensure_contact(ContactNodes) ->
  Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      {error, no_contact_nodes_reachable};

    _ ->
      DefaultTime = 6000,
      WaitTime = get_env(simple_cache, wait_time, DefaultTime),
      wait_for_nodes(length(Answering), WaitTime)
end.

wait_for_nodes(MinNodes, WaitTime) ->
  Slices = 10,
  SliceTime = round(WaitTime/Slices),
  wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
  ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
  case length(nodes()) > MinNodes of
    true ->
      ok;

    _ ->
      timer:sleep(SliceTime),
      wait_for_nodes(MinNodes, SliceTime, Iterations -1)
  end.

get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    {ok, Value} ->
      Value;

    _ ->
      Default
  end.
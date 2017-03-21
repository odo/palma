-module(palma_pool_sup).
-behaviour(palma_supervisor2).

-export([start_link/2, init/1, start_child/2]).

start_link(Id, Params) ->
    palma_supervisor2:start_link({local, Id}, ?MODULE, Params).

start_child(Supervisor, Params) ->
    palma_supervisor2:start_child(Supervisor, Params).

init(ChildSpec) ->
    {ok, {{simple_one_for_one, 100, 1}, [ChildSpec]}}.

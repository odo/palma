-module(palma_pool_sup).
-behaviour(supervisor2).

-export([start_link/2, init/1, start_child/2]).

start_link(Id, Params) ->
    supervisor2:start_link({local, Id}, ?MODULE, Params).

start_child(Supervisor, Params) ->
    supervisor2:start_child(Supervisor, Params).

init(ChildSpec) ->
    {ok, {{simple_one_for_one, 1, 1}, [ChildSpec]}}.


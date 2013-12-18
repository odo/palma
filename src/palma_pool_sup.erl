-module(palma_pool_sup).
-behaviour(supervisor).

-export([start_link/2, init/1, start_child/2]).

start_link(Id, Params) ->
    supervisor:start_link({local, Id}, ?MODULE, Params).

start_child(Supervisor, Params) ->
    supervisor:start_child(Supervisor, Params).

init(ChildSpec) ->
    {ok, {{simple_one_for_one, 10, 10}, [ChildSpec]}}.


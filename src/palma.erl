-module(palma).

-export([start/0, new/3, pid/1]).

start() ->
    application:start(palma).

new(PoolName, PoolSize, ChildSpec) ->
    SupervisorName   = list_to_atom(atom_to_list(PoolName) ++ "_sup"),
    {Id, {M, F, ChildInitArgs}, Restart, Shutdown, Type, Modules} = ChildSpec,
    ChildSpecNew     = {Id, {M, F, []}, Restart, Shutdown, Type, Modules},
    {ok, Supervisor} = supervisor:start_child(
        palma_sup,
        {SupervisorName, {palma_pool_sup, start_link, [SupervisorName, ChildSpecNew]}, permanent, 10000, supervisor, [palma_pool_sup]}),
    [palma_pool_sup:start_child(Supervisor, ChildInitArgs) || _ <- lists:seq(1, PoolSize)],
    ballermann:balance(Supervisor, PoolName).

pid(PoolName) ->
    ballermann:pid(PoolName).

-module(palma).

-export([start/0, new/3, new/4, new/5, stop/1, pid/1, pid/2, transaction/2, transaction/3]).

-define(DEFAULTSHUTDOWNDELAY, 10000).
-define(DEFAULTREVOLVEROPTIONS, #{ min_alive_ratio => 1.0, reconnect_delay => 1000}).

start() ->
    application:ensure_all_started(palma).

new(PoolName, PoolSize, ChildSpec) ->
    new(PoolName, PoolSize, ChildSpec, ?DEFAULTSHUTDOWNDELAY).

new(PoolName, PoolSize, ChildSpec, ShutdownDelay) ->
    new(PoolName, PoolSize, ChildSpec, ShutdownDelay, ?DEFAULTREVOLVEROPTIONS).

new(PoolName, PoolSize, ChildSpec, ShutdownDelay, RevolverOptions) ->
    {Id, {M, F, ChildInitArgs}, Restart, Shutdown, Type, Modules} = ChildSpec,
    ChildSpecNew     = {Id, {M, F, []}, Restart, Shutdown, Type, Modules},
    SupervisorName   = supervisor_name(PoolName),
    {ok, Supervisor} = supervisor:start_child(
        palma_sup,
        {SupervisorName, {palma_pool_sup, start_link, [SupervisorName, ChildSpecNew]}, permanent, ShutdownDelay, supervisor, [palma_pool_sup]}),
    [palma_pool_sup:start_child(Supervisor, ChildInitArgs) || _ <- lists:seq(1, PoolSize)],
    supervisor:start_child(
      palma_sup,
      revolver_sup:child_spec(SupervisorName, PoolName, RevolverOptions)
     ).

stop(PoolName) ->
    supervisor:terminate_child(palma_sup, revolver_utils:supervisor_name(PoolName)),
    supervisor:delete_child(palma_sup, revolver_utils:supervisor_name(PoolName)),
    supervisor:terminate_child(palma_sup, supervisor_name(PoolName)),
    supervisor:delete_child(palma_sup, supervisor_name(PoolName)).

pid(PoolName) when is_atom(PoolName) or is_pid(PoolName) ->
    revolver:pid(PoolName);

pid([PoolName | _] = NewArgs) when is_list(NewArgs)->
    case whereis(PoolName) of
        undefined ->
            apply(?MODULE, new, NewArgs),
            pid(PoolName);
        PoolPid ->
            pid(PoolPid)
    end.

pid(CallbackModule, Options) when is_atom(CallbackModule) ->
    PoolName = apply(CallbackModule, pool_name, [Options]),
    case whereis(PoolName) of
        undefined ->
            case apply(CallbackModule, init_all,  [Options], true) of
                false ->
                    {error, {init_all, failed}};
                true ->
                    PoolSize        = apply(CallbackModule, pool_size,        [Options]),
                    ChildSpec       = apply(CallbackModule, child_spec,       [Options]),
                    ShutdownDelay   = apply(CallbackModule, shutdown_delay,   [Options], ?DEFAULTSHUTDOWNDELAY),
                    RevolverOptions = apply(CallbackModule, revolver_options, [Options], ?DEFAULTREVOLVEROPTIONS),
                    new(PoolName, PoolSize, ChildSpec, ShutdownDelay, RevolverOptions),
                    pid(PoolName)
                end;
        PoolPid ->
            pid(PoolPid)
    end.

transaction(PoolName, Fun) when (is_atom(PoolName) or is_pid(PoolName)) and is_function(Fun) ->
  revolver:transaction(PoolName, Fun).

transaction(PoolName, Fun, KillOnError) when (is_atom(PoolName) or is_pid(PoolName)) and is_function(Fun) and is_boolean(KillOnError) ->
  revolver:transaction(PoolName, Fun, KillOnError).

supervisor_name(PoolName) ->
    list_to_atom(atom_to_list(PoolName) ++ "_sup").


apply(Module, Function, Args, Default) ->
    try apply(Module, Function, Args) of
        Value -> Value
    catch
        error:undef ->
            Default
    end.

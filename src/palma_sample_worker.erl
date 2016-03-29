-module(palma_sample_worker).

-behaviour(palma_callback).
-export([pool_name/1, init_all/1, pool_size/1, child_spec/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% Palma Callbacks

pool_name(Options) ->
  maps:get(name, Options).

init_all(_) ->
  io:format("doing something global here...\n", []),
  true.

pool_size(_) ->
  10.

child_spec(Options) ->
  InitialState = maps:get(initial_state, Options),
  {my_id, {palma_sample_worker, start_link, [InitialState]}, permanent, 1000, worker, [?MODULE]}.

%% Public API

start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

%% GenServer Callbacks

init(State) ->
    {ok, State}.

handle_call(state, _From, State) ->
    {reply, {self(), State}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

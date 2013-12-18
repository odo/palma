-module(palma_sample_worker).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% Public API

start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

%% Callbacks

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

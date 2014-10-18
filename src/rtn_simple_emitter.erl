%%% -*- erlang -*-
%%%
%%% This file is part of routy released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(rtn_simple_emitter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(INTERVAL, 5000). % 5s

-include("routy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(NodeId) ->
    gen_server:start_link(?MODULE, [NodeId], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([NodeId]) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, NodeId}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, NodeId) ->
    % send the message
    NodeMsg = #rmsg{
        from = NodeId,
        attributes = [{foo, bar}]
    },
    routy_router:route_msg(NodeMsg),
    % start new timer
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, NodeId};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
    

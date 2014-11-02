%% -*- erlang -*-
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(rtn_simple_emitter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(INTERVAL, 5000). % 5s

-include("routy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(NodeId, Args) ->
    gen_server:start_link(?MODULE, [NodeId, Args], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([NodeId, [Interval]]) ->
    erlang:send_after(Interval, self(), trigger),
    {ok, [NodeId, Interval]}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, [NodeId, Interval]=State) ->
    % send the message
    NodeMsg = #rmsg{
        from = NodeId,
        attributes = [{foo, bar}]
    },
    routy_network:route_msg(NodeMsg),
    % start new timer
    erlang:send_after(Interval, self(), trigger),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
    

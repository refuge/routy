%%% -*- erlang -*-
%%%
%%% This file is part of routy released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(routy_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("routy.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([route_msg/1]).
-export([add_flow/2, remove_flow/2]).
-export([dump/0]).
-export([register_node/2, unregister_node/1]).
-export([killall/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    io:format("Launching Routy router!~n"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ---

route_msg(NodeMsg) ->
    gen_server:cast(?MODULE, {route_msg, NodeMsg}).

dump() ->
    gen_server:cast(?MODULE, {dump}).

%% ---

add_flow(FromNode, ToNode) ->
    gen_server:call(?MODULE, {add_flow, FromNode, ToNode}).

remove_flow(FromNode, ToNode) ->
    gen_server:call(?MODULE, {remove_flow, FromNode, ToNode}).

register_node(NodeId, Pid) ->
    gen_server:call(?MODULE, {register_node, NodeId, Pid}).

unregister_node(NodeId) ->
    gen_server:call(?MODULE, {unregister_node, NodeId}).

%% ---

killall() ->
    gen_server:call(?MODULE, {killall}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(
    rrstate,
    {
        nodePids = #{},
        flows = #{}
    }
).

init(_) ->
    {ok, #rrstate{}}.

handle_call({killall}, _From, #rrstate{nodePids=NodePids}) ->
    ok = kill_nodes(NodePids),
    {reply, ok, #rrstate{}};
handle_call({add_flow, FromNode, ToNode}, _From, #rrstate{flows=Flows}=State) ->
    NewState = case maps:find(FromNode, Flows) of
        {ok, ReceivingNodes} ->
            case lists:member(ToNode, ReceivingNodes) of
                true ->
                    %% gracefuly failing: the receiving node is already registered
                    State;
                false ->
                    UpdatedReceivingNodes = [ToNode|ReceivingNodes],
                    State#rrstate{flows=maps:put(FromNode, UpdatedReceivingNodes, Flows)}
            end;
        error ->
            State#rrstate{flows=maps:put(FromNode, [ToNode], Flows)}
    end,
    {reply, ok, NewState};
handle_call({remove_flow, FromNode, ToNode}, _From, #rrstate{flows=Flows}=State) ->
    NewState = case maps:find(FromNode, Flows) of
        {ok, ReceivingNodes} ->
            UpdatedReceivingNodes = lists:delete(ToNode, ReceivingNodes),
            State#rrstate{flows=maps:put(FromNode, UpdatedReceivingNodes, Flows)};
        error ->
            %% gracefuly failing: the flow doesn't exist
            State
    end,
    {reply, ok, NewState};
handle_call({register_node, NodeId, Pid}, _From, #rrstate{nodePids=NodePids}=State) ->
    NewState = State#rrstate{nodePids=maps:put(NodeId, Pid, NodePids)},
    {reply, ok, NewState};
handle_call({unregister_node, NodeId}, _From, #rrstate{nodePids=NodePids}=State) ->
    NewState = State#rrstate{nodePids=maps:remove(NodeId, NodePids)},
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({route_msg, #rmsg{from=FromNode}=NodeMsg}, #rrstate{nodePids=NodePids, flows=Flows}=State) ->
    case maps:find(FromNode, Flows) of
        {ok, ReceivingNodes} ->
            lists:foldl(
                fun(ToNode, _Acc) ->
                    io:format("Routing message [~p] from ~p to ~p~n", [NodeMsg, FromNode, ToNode]),
                    send_message(NodeMsg, ToNode, NodePids),
                    []
                end,
                [],
                ReceivingNodes
            );
        error ->
            io:format("Can't route message [~p]!~n", [NodeMsg])
    end,
    {noreply, State};
handle_cast({dump}, #rrstate{nodePids=NodePids, flows=Flows}=State) ->
    io:format("Routy router dump:~n~n"),
    ok = dump_node_pids(NodePids),
    ok = dump_flows(Flows),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_message(NodeMsg, NodeId, NodePids) ->
    case maps:find(NodeId, NodePids) of
        {ok, Pid} ->
            gen_server:cast(Pid, {process_message, NodeMsg}),
            ok;
        error ->
            {error, unknown_node}
    end.

dump_flows(Flows) ->
    io:format("Flows:~n~n"),
    maps:fold(
        fun(FromNode, ReceivingNodes, _Acc) ->
            io:format("Flows from ~p to ~p~n", [FromNode, ReceivingNodes]),
            []
        end,
        [],
        Flows
    ),
    ok.

dump_node_pids(NodePids) ->
    io:format("Nodes:~n~n"),
    maps:fold(
        fun(NodeId, Pid, _Acc) ->
            io:format("Node with id ~p is PID ~p~n", [NodeId, Pid]),
            []
        end,
        [],
        NodePids
    ),
    ok.

kill_nodes(NodePids) ->
    maps:fold(
        fun(NodeId, Pid, _Acc) ->
            io:format("KILLING Node with id ~p is PID ~p~n", [NodeId, Pid]),
            gen_server:call(Pid, stop),
            []
        end,
        [],
        NodePids
    ),
    ok.

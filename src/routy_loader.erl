%%% -*- erlang -*-
%%%
%%% This file is part of routy released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(routy_loader).

-export([load_flows/1]).
-export([start_example/0]).
-export([load_example/0]).

% {
%     nodes=[{1, module_name, [Arguments]}, {...}],
%     flows=[{1,2}, {...}]
% }

load_flows({Nodes, Flows}) ->
    % kill everything if needed
    routy_router:killall(),

    % load the scenario
    create_nodes(Nodes),
    create_flows(Flows),

    % dump the current state
    routy_router:dump(),

    ok.

start_example() ->
    lager:start(),
    application:start(routy),
    load_example().

load_example() ->
    load_flows( {
            [{10, rtn_simple_emitter, [10000]}, {20, rtn_simple_receiver, []}],
            [{10,20}]
        }
    ).

%% ----

create_nodes(Nodes) ->
    lists:foldl(
        fun({NodeId, Module, Args}, _) ->
            lager:info("Create Node[~p] with module: ~p~n", [NodeId, Module]),
            {ok, Pid} = Module:start_link(NodeId, Args),
            routy_router:register_node(NodeId, Pid),
            []
        end,
        [],
        Nodes
    ),
    ok.

create_flows(Flows) ->
    lists:foldl(
        fun({FromNode, ToNode}, _) ->
            routy_router:add_flow(FromNode, ToNode),
            []
        end,
        [],
        Flows
    ),
    ok.

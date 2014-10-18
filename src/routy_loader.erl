%%% -*- erlang -*-
%%%
%%% This file is part of routy released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(routy_loader).

-export([load_flows/1]).
-export([load_example/0]).

% {
%     nodes=[{1, simple_...}, {...}],
%     flows=[{1,2}, {...}]
% }

load_flows({Nodes, Flows}) ->
    % kill everything if needed
    routy_router:killall(),

    % load the scenario
    create_nodes(Nodes),
    create_flows(Flows),
    ok.

load_example() ->
    load_flows( {
            [{1, rtn_simple_emitter}, {2, rtn_simple_receiver}],
            [{1,2}]
        }
    ).

%% ----

create_nodes(Nodes) ->
    lists:foldl(
        fun({NodeId, Module}, _) ->
            {ok, Pid} = Module:start_link(),
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

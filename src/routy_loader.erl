%% -*- erlang -*-
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(routy_loader).

-export([load_network/1, load_network/2]).

-include("routy.hrl").

load_network(Network) ->
    load_network(Network, true).

load_network(#rnetwork{nodes=Nodes, flows=Flows}, Dump) ->
    % kill everything if needed
    routy_network:killall(),

    % load the scenario
    create_nodes(Nodes),
    create_flows(Flows),

    % dump the current state
    case Dump of
        true -> routy_network:dump()
    end,

    ok.

%% ----

create_nodes(Nodes) ->
    lists:foldl(
        fun({NodeId, Module, Args}, _) ->
            lager:info("Create Node[~p] with module: ~p~n", [NodeId, Module]),
            {ok, Pid} = Module:start_link(NodeId, Args),
            routy_network:register_node(NodeId, Pid),
            []
        end,
        [],
        Nodes
    ),
    ok.

create_flows(Flows) ->
    lists:foldl(
        fun({FromNode, ToNode}, _) ->
            routy_network:add_flow(FromNode, ToNode),
            []
        end,
        [],
        Flows
    ),
    ok.

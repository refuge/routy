%% -*- erlang -*-
%%
%%  This Source Code Form is subject to the terms of the Mozilla Public
%%  License, v. 2.0. If a copy of the MPL was not distributed with this
%%  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(routy_example).

-export([start_example/0]).
-export([deploy_example/0]).

-include("routy.hrl").

start_example() ->
    lager:start(),
    application:start(routy),
    deploy_example().

deploy_example() ->
    Network = #rnetwork{
            nodes=[{10, rtn_simple_emitter, [10000]}, {20, rtn_simple_receiver, []}],
            flows=[{10,20}]
    },
    route_deployer:deploy_network(Network).


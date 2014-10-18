%%% -*- erlang -*-
%%%
%%% This file is part of routy released under the Apache 2 license.
%%% See the NOTICE for more information.

-module(routy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    routy_sup:start_link().

stop(_State) ->
    ok.

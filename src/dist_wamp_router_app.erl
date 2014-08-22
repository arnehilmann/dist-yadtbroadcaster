%% @private
-module(dist_wamp_router_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  dist_wamp_router:start_link().

stop(_State) ->
	ok.

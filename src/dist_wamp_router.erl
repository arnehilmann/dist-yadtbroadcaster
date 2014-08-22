%% @private
-module(dist_wamp_router).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link( ?MODULE, []).

%% supervisor.

init([]) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", cowboy_static, {priv_file, dist_wamp_router, "index.html"}},
                                           {"/wamp", my_ws_handler, []},
                                           %{"/wamp", erwa_ws_handler, []},
                                           {"/static/[...]", cowboy_static, {priv_dir, dist_wamp_router, "static"}}
                                           ]}
                                    ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]),
  {ok,_} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),
  {ok, {{one_for_one, 10, 10}, []}}.

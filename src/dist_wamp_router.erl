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
  {ok, RanchPid} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),
  io:format("ranch result: ~p~n", [RanchPid]),
  register(ranch, RanchPid),
  net_adm:ping('dist_wamp_router@node1'),
  net_adm:ping('dist_wamp_router@node2'),
  net_adm:ping('dist_wamp_router@node3'),
  N2 = hd(nodes()),
  R2 = rpc:call(N2, erlang, whereis, [ranch]),
  io:format("ranch on ~p: ~p~n", [N2, R2]),
  {ok, {{one_for_one, 10, 10}, []}}.

%% @private
-module(dist_wamp_router).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).


-define(DOCROOT, "/var/lib/yadt-broadcaster/docroot").

%% API.
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link( ?MODULE, []).

%% supervisor.
init([]) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/status", status_handler, []},
                                             {"/api/[...]", rest_api_handler, []},
                                             {"/wamp", yadt_ws_handler, []},
                                             {"/", cowboy_static, {file, ?DOCROOT ++ "/index.html"}},
                                             {"/[...]", cowboy_static, {dir, ?DOCROOT}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[
                                                           {env, [{dispatch, Dispatch}]}
                                                          ]),
    {ok, _} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),

    ok = message_forwarder:start_listener(),

    {ok, Peers} = cluster_connect:read_peers(),
    ok = cluster_connect:ping_peers(Peers),
    io:format("nodes responding: ~p~n", [nodes()]),

    {ok, {{one_for_one, 10, 10}, []}}.

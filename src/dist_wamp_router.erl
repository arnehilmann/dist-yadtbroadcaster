%% @private
-module(dist_wamp_router).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).


%-export([listen_for_forwards/0]).


-define(WSKEY, {pubsub, wsbroadcast}).

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
                                             {"/[...]", cowboy_static, {dir, "/var/lib/yadt-broadcaster/docroot"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[
                                                           {env, [{dispatch, Dispatch}]}
                                                          ]),
    {ok, _} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),

    %state_store:store(["targets", "__dummy__", "__state__"], "PRESENT"),

    ok = message_forwarder:start_listener(),
    %ForwardListener = erlang:spawn_link(?MODULE, listen_for_forwards, []),
    %register(forwards, ForwardListener),

    %{ok, Dir} = file:get_cwd(),
    %io:format('~p~n', [Dir]),

    {ok, Peers} = cluster_connect:read_peers(),
    %io:format('~p~n', [Peers]),

    ok = cluster_connect:ping_peers(Peers),
    io:format("nodes responding: ~p~n", [nodes()]),
    {ok, {{one_for_one, 10, 10},
    [
      {state_store,
        {state_store, start_link, []},
        permanent,
        5000,
        worker,
        [state_store]
      }
    ]}}.

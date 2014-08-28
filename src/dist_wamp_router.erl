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

    {ok, Dir} = file:get_cwd(),
    io:format('~p~n', [Dir]),

    {ok, Peers} = read_peers(),
    io:format('~p~n', [Peers]),

    ok = ping_peers(Peers),
    io:format("nodes responding: ~p~n", [nodes()]),
    %N2 = hd(nodes()),
    %R2 = rpc:call(N2, erlang, whereis, [ranch]),
    %io:format("ranch on ~p: ~p~n", [N2, R2]),
    {ok, {{one_for_one, 10, 10}, []}}.


ping_peers([]) ->
    ok;
ping_peers([H|T]) ->
    io:format('host to ping: ~p~n', [H]),
    io:format('remaining hosts: ~p~n~n', [T]),
    NodeName = list_to_atom(atom_to_list(erlang:get_cookie()) ++ "@" ++ H),
    io:format('node to ping: ~p~n', [NodeName]),
    net_adm:ping(NodeName),
    ping_peers(T).

read_peers() ->
    case file:read_file("/host-wd/peers") of
        {error, Reason} ->      io:format("cannot read peers file: ~p~n", [Reason]), 
                                {ok, []};
        {ok, FileContent} ->    Peers = string:tokens(binary_to_list(FileContent), ", \n"),
                                {ok, Peers}
    end.


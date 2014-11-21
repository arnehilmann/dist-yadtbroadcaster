%% @private
-module(dist_wamp_router).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).


-export([listen_for_forwards/0]).


-define(WSKEY, {pubsub, wsbroadcast}).

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
                                             {"/static/[...]", cowboy_static, {priv_dir, dist_wamp_router, "static"}}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],[{env, [{dispatch, Dispatch}]}]),
    {ok, _} = ranch:start_listener(erwa_tcp, 5, ranch_tcp, [{port,5555}], erwa_tcp_handler, []),

    {ok, _} = state_store:start_link(),
    state_store:store(["yadt", "bar", "baz"], "UNKNOWN"),

    ForwardListener = erlang:spawn_link(?MODULE, listen_for_forwards, []),
    register(forwards, ForwardListener),

    {ok, Dir} = file:get_cwd(),
    io:format('~p~n', [Dir]),

    {ok, Peers} = read_peers(),
    io:format('~p~n', [Peers]),

    ok = ping_peers(Peers),
    io:format("nodes responding: ~p~n", [nodes()]),
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
    case file:read_file("/docker/peers") of
        {error, Reason} ->      io:format("cannot read peers file: ~p~n", [Reason]),
                                {ok, []};
        {ok, FileContent} ->    Peers = string:tokens(binary_to_list(FileContent), ", \n"),
                                {ok, Peers}
    end.

listen_for_forwards() ->
    receive
        {From, Realm, Data} ->
            io:format("received data from ~p on realm ~p:~n~p~n", [From, Realm, Data]),
            forward_message(Data, Realm)
    end,
    listen_for_forwards().

forward_message(Message, Realm) when is_bitstring(Realm) ->
  case erwa_realms:get_router(Realm) of
    {ok,Pid} ->
      forward_message(Message,Pid);
    {error,not_found} ->
      self() ! {erwa,{abort,[{}],no_such_realm}},
      self() ! {erwa, shutdown},
      {error,undefined}
  end;
forward_message(Message, Router) when is_pid(Router) ->
  Router ! {forwarded, Message},
  ok;
forward_message(AnyData, AnyObject) ->
    io:format("cannot handle realm/router ~p~ndropping data ~p~n", [AnyObject, AnyData]),
    ok.

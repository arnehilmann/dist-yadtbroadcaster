-module(cluster_connect).

%% API.
-export([read_peers/0]).
-export([ping_peers/1]).

ping_peers([]) ->
    ok;
ping_peers([H|T]) ->
    NodeName = list_to_atom(atom_to_list(erlang:get_cookie()) ++ "@" ++ H),
    io:format('pinging node ~s~n', [NodeName]),
    net_adm:ping(NodeName),
    ping_peers(T).

read_peers() ->
    case file:read_file("/etc/sysconfig/dist-wamp-router.nodes") of
        {error, Reason} ->      io:format("cannot read peers file: ~p~n", [Reason]),
                                {ok, []};
        {ok, FileContent} ->    Peers = string:tokens(binary_to_list(FileContent), ", \n"),
                                {ok, Peers}
    end.

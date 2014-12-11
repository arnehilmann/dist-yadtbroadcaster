-module(state_store).

-export([store/2]).
-export([fetch/2]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

store(Where, What) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("preparing to put something to ~s~n", [Url]),
    try hackney:put(Url, [{<<"Content-Type">>, <<"text/plain">>}], What, [{pool, storepool}, {recv_timeout, 5000}, {connect_timeout, 5000}]) of
        {ok, StatusCode, _RespHeaders, _ClientRef} ->
            io:format("put returned with status code ~p~n", [StatusCode]),
            if
                StatusCode >= 300 -> io:format("status of store: ~p~n", [StatusCode]);
                true -> ok
            end;
        _ ->
            io:format("something bad happened here~n")
    catch
        _ ->
            io:format("an exception occured~n")
    end,
    io:format("store to ~s completed~n", [Url]).

fetch(Where, From) ->
    Url = list_to_binary(io_lib:format(?RIAK_URL, Where)),
    io:format("fetching info from url: ~s~n", [Url]),
    case hackney:get(Url, [], <<"">>, [{recv_timeout, 5000}, {pool, fetchpool}]) of
        {error, Reason} ->
            io:format("Problem while fetching ~p: ~p~n", [Url, Reason]),
            From ! {error, Reason};
        {ok, _StatusCode, _ResponseHeaders, ClientRef} ->
            io:format("first response received, querying body now~n"),
            {ok, Response} = hackney:body(ClientRef),
            io:format("response from ~s is:~n~p~n", [Url, Response]),
            From ! {ok, Response},
            io:format("response sent back to ~p~n", [From])
    end.

-module(state_store).

-export([store/2]).
-export([fetch/2]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

store(Where, What) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("preparing to put something to ~s~n", [Url]),
    {ok, {Status, _Header, _Body}} = httpc:request(put, {Url, [], "text/plain", What}, [], []),
    io:format("response: ~p~n", [Status]).

fetch(Where, From) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("fetching info from url ~s~n", [Url]),
    case httpc:request(Url) of
        {error, Reason} ->
            io:format("Problem while fetching ~p: ~p~n", [Url, Reason]),
            From ! {error, Reason};
        {ok, {Status, _Header, Body}} ->
            {_, Code, Reason} = Status,
            if
                Code >= 300 ->
                    io:format("error response: ~p~n", [Status]),
                    From ! {error, Reason};
                true ->
                    io:format("response from ~s is ~p~n", [Url, Body]),
                    From ! {ok, Body}
            end
    end.

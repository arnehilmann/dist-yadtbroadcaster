-module(state_store).

-export([store/2]).
-export([fetch/2]).
-export([delete/1]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

store(Where, What) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("preparing to put something to ~s~n", [Url]),
    Response = httpc:request(put, {Url, [], "text/plain", What}, [], []),
    handle_response_or_error(Url, Response, noreply).

fetch(Where, From) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("fetching info from url ~s~n", [Url]),
    Response = httpc:request(Url),
    handle_response_or_error(Url, Response, From).

delete(Where) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("deleting content at url ~s~n", [Url]),
    Response = httpc:request(delete, {Url, []}, [], []),
    handle_response_or_error(Url, Response, noreply).

handle_response_or_error(Url, {error, Reason}, From) ->
    io:format("Problem while invoking ~s: ~p~n", [Url, Reason]),
    reply_if_required({error, Reason}, From);

handle_response_or_error(Url, {ok, {Status, _Header, Body}}, From) ->
    {_, Code, Reason} = Status,
    if
        Code >= 300 ->
            io:format("error response: ~p~n", [Status]),
            reply_if_required({error, Reason}, From);
        true ->
            io:format("response from ~s has length ~p and code ~p~n", [Url, string:len(Body), Code]),
            reply_if_required({ok, Body}, From)
    end.

reply_if_required(_What, noreply) ->
    ok;
reply_if_required(What, From) ->
    From ! What,
    ok.

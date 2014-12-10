-module(state_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([store/2]).
-export([fetch/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([fetch_from_db/2]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Where, What) ->
    gen_server:cast(?MODULE, {store, Where, What}).

fetch(Where, From) ->
    gen_server:cast(?MODULE, {fetch, Where, From}).

init([]) ->
    ok = hackney_pool:start_pool(storepool, [{timeout, 150000}, {max_connections, 100}]),
    ok = hackney_pool:start_pool(fetchpool, [{timeout, 150000}, {max_connections, 100}]),
    {ok, []}.

handle_call(Args, From, _State) ->
    io:format("unrecognized call with args ~p from ~p~n", [Args, From]),
    {noreply, []}.

fetch_from_db(Where, From) ->
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

handle_cast({fetch, Where, From}, State) ->
    spawn(?MODULE, fetch_from_db, [Where, From]),
    {noreply, State};
handle_cast({store, Where, What}, State) ->
    Url = io_lib:format(?RIAK_URL, Where),
    %io:format("~s -> ~s~n", [Url, What]),
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
    io:format("store to ~s completed~n", [Url]),
    {noreply, State};
handle_cast(Args, State) ->
    io:format("something went wrong here: ~p~n~p~n", [Args, State]),
    {noreply, State}.

handle_info(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    ok.

terminate(_, _) ->
    ok.

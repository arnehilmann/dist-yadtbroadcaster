-module(state_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([store/2]).
-export([fetch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Where, What) ->
    gen_server:cast(?MODULE, {store, Where, What}).

fetch(Where) ->
    gen_server:call(?MODULE, {fetch, Where}).

init([]) ->
    ok = hackney_pool:start_pool(storepool, [{timeout, 150000}, {max_connections, 100}]),
    ok = hackney_pool:start_pool(fetchpool, [{timeout, 150000}, {max_connections, 100}]),
    {ok, []}.

handle_call({fetch, Where}, _From, State) ->
    %io:format("fetching info from ~p~n", [Where]),
    Url = list_to_binary(io_lib:format(?RIAK_URL, Where)),
    io:format("fetching info from url: ~s~n", [Url]),
    case hackney:get(Url, [], <<"">>, [{recv_timeout, 5000}, {pool, fetchpool}]) of
        {error, Reason} -> io:format("Problem while fetching ~p: ~p~n", [Url, Reason]),
                           {reply, {error, Reason}, State};
        {ok, _StatusCode, _ResponseHeaders, ClientRef} ->
                           io:format("first response received, querying body now~n"),
                           {ok, Response} = hackney:body(ClientRef),
                           {reply, {ok, Response}, State}
    end;
handle_call(Args, From, _State) ->
    io:format("unrecognized call with args ~p from ~p~n", [Args, From]),
    {noreply, []}.

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

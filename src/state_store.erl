-module(state_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([store/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(RIAK_URL,"http://localhost:8098/types/~s/buckets/~s/keys/~s").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Where, What) ->
    gen_server:cast(?MODULE, {store, Where, What}).

init([]) ->
    {ok, []}.

handle_call(Args, From, _State) ->
    io:format("unrecognized call with args ~p from ~p~n", [Args, From]),
    {noreply, []}.

handle_cast({store, Where, What}, State) ->
    Url = io_lib:format(?RIAK_URL, Where),
    io:format("~s -> ~s~n", [Url, What]),
    {ok, StatusCode, _RespHeaders, _ClientRef} = hackney:put(
        Url, [{<<"Content-Type">>, <<"text/plain">>}], What, []
    ),
    if
        StatusCode >= 300 ->
            io:format("status of store: ~p~n", [StatusCode]);
        true ->
            ok
    end,
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

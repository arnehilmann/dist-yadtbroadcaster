%% @private
-module(rest_api_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    io:format("path info: ~p~n", [PathInfo]),

    {ok, Req2} = handle_rest_api_call(PathInfo, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.


handle_rest_api_call([<<"v1">>|Path], Req) ->
    rest_api_handler_v1:handle_rest_api_call(Path, Req);
handle_rest_api_call(Path, Req) ->
    io:format("cannot process ~p: unknown api version~n", [Path]),
    cowboy_req:reply(
        404,
        [ {<<"content-type">>, <<"text/plain">>} ],
        list_to_binary(io_lib:format("cannot process ~p", [Path])),
        Req).


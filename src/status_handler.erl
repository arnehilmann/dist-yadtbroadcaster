%% @private
-module(status_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, _State) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    io:format("path info: ~p~n", [PathInfo]),
    reply(<<"ok">>, Req).

reply(Response, Req) ->
    cowboy_req:reply(
    200,
    [ {<<"content-type">>, <<"text/plain">>} ],
    Response,
    Req).

terminate(_Reason, _Req, _State) ->
    ok.

%% @private
-module(status_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    cowboy_req:reply(
      200,
      [ {<<"content-type">>, <<"text/plain">>} ],
      <<"ok">>,
      Req
     ),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

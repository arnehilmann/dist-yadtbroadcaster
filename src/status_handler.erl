%% @private
-module(status_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(RIAK_STATS_URL, "http://localhost:8098/stats").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, {Status, _Header, Body}} = httpc:request(?RIAK_STATS_URL),
    {_HttpVersion, Code, _Text} = Status,
    CompleteBody = "{\"broadcast-status\": \"ok\", \"riak-status\": " ++ Body ++ "}",
    PrettyBody = jsx:prettify(list_to_binary(CompleteBody)),
    cowboy_req:reply(
      Code,
      [ {<<"content-type">>, <<"text/plain">>} ],
      PrettyBody,
      Req
     ),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.


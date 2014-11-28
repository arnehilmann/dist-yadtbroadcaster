-module(cors_enabler).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
{ok, ReqWithCorsHeaders} = set_cors_headers(Req),
{Method, ReqMethod} = cowboy_req:method(ReqWithCorsHeaders),
 
case Method of
<<"OPTIONS">> ->
{ok, ReqFinal} = cowboy_req:reply(200, ReqMethod),
{halt, ReqFinal};
_ ->
%% continue as normal
{ok, ReqMethod, Env}
end.
 
%% ===================================================================
%% Helpers
%% ===================================================================
 
set_headers(Headers, Req) ->
ReqWithHeaders = lists:foldl(fun({Header, Value}, ReqIn) ->
ReqWithHeader = cowboy_req:set_resp_header(Header, Value, ReqIn),
ReqWithHeader
end, Req, Headers),
{ok, ReqWithHeaders}.
 
set_cors_headers(Req) ->
Headers = [{<<"access-control-allow-origin">>, <<"*">>},
{<<"access-control-allow-methods">>, <<"POST, GET, OPTIONS">>},
{<<"access-control-allow-headers">>, <<"Origin, X-Requested-With, Content-Type, Accept">>},
{<<"access-control-max-age">>, <<"1000">>}],
{ok, Req2} = set_headers(Headers, Req),
{ok, Req2}. 

%% @private
-module(status_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    %io:format("request: ~n~p~n", [Req]),
    {Url, _} = cowboy_req:url(Req),
    io:format("url: ~n~p~n", [Url]),
    {Path, _} = cowboy_req:path(Req),
    io:format("path: ~n~p~n", [Path]),
    {PathInfo, _} = cowboy_req:path_info(Req),
    io:format("path info: ~n~p~n", [PathInfo]),

    {ok, Req2} = handle_status_req(PathInfo, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.



handle_status_req([], Req) ->
    reply(<<"ok">>, Req);
handle_status_req([<<"services">>, Host, ServiceName], Req) ->
    {ok, Response} = state_store:fetch(["services", Host, ServiceName]),
    reply(Response, Req);
handle_status_req([<<"hosts">>, Host, <<"services">>], Req) ->
    {ok, Response} = state_store:fetch(["hosts", Host, "services"]),
    reply(Response, Req);
handle_status_req([<<"targets">>, Target, <<"hosts">>], Req) ->
    {ok, Response} = state_store:fetch(["targets", Target, "hosts"]),
    reply(Response, Req);
handle_status_req([<<"targets">>, Target, <<"full">>], Req) ->
    {ok, Hosts} = state_store:fetch(["targets", Target, "hosts"]),
    io:format("hosts: ~p~n", [binary_to_list(Hosts)]),
    io:format("tokens: ~p~n", [string:tokens(binary_to_list(Hosts), "\n")]),
    Responses = lists:map(fun (Host) ->
                                  {ok, ServicesString} = state_store:fetch([<<"hosts">>, Host, <<"services">>]),
                                  Services = binary:split(ServicesString, <<"\n">>, [global]),
                                  ServiceStates = lists:map(fun (Service) ->
                                                                    {ok, State} = state_store:fetch([<<"services">>, Host, Service]),
                                                                    [{<<"service">>, Service}, {<<"state">>, State}]
                                                            end,
                                                            Services
                                                           ),
                                  [{<<"host">>, Host}, {<<"services">>, ServiceStates}]
                          end,
                          binary:split(Hosts, <<"\n">>, [global])),
    io:format("response:~n~p~n", [Responses]),
    io:format("response:~n~p~n", [jsx:encode(Responses)]),
    reply(jsx:prettify(jsx:encode(Responses)), Req);
handle_status_req(Path, Req) ->
    cowboy_req:reply(
      404,
      [ {<<"content-type">>, <<"text/plain">>} ],
      list_to_binary(io_lib:format("no status found for ~p", [Path])),
      Req).

reply(Response, Req) ->
    cowboy_req:reply(
      200,
      [ {<<"content-type">>, <<"text/plain">>} ],
      Response,
      Req).

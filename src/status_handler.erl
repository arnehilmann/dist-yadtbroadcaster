%% @private
-module(status_handler).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {PathInfo, _} = cowboy_req:path_info(Req),
    io:format("path info: ~p~n", [PathInfo]),

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
    Responses = lists:map(fun (Host) ->
                                  {ok, ServicesString} = state_store:fetch([<<"hosts">>, Host, <<"services">>]),
                                  Services = binary:split(ServicesString, <<"\n">>, [global]),
                                  ServiceStates = lists:map(fun (Service) ->
                                                                    {ok, State} = state_store:fetch([<<"services">>, Host, Service]),
                                                                    [{<<"name">>, Service}, {<<"state">>, State}]
                                                            end,
                                                            Services
                                                           ),
                                  {ok, ArtefactsString} = state_store:fetch([<<"hosts">>, Host, <<"artefacts">>]),
                                  ArtefactsList = binary:split(ArtefactsString, <<"\n">>, [global]),
                                  Artefacts = lists:map(fun (ArtefactItem) ->
                                                                [Name, Version] = binary:split(ArtefactItem, <<" ">>),
                                                                [{<<"name">>, Name}, {<<"version">>, Version}]
                                                        end,
                                                        ArtefactsList),
                                  [{<<"host">>, Host}, {<<"services">>, ServiceStates}, {<<"artefacts">>, Artefacts}]
                          end,
                          binary:split(Hosts, <<"\n">>, [global])),
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

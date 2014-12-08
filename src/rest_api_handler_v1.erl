%% @private
-module(rest_api_handler_v1).
-export([handle_rest_api_call/2]).

handle_rest_api_call([], Req) ->
    reply(<<"ok">>, Req);
handle_rest_api_call([<<"services">>, Host, ServiceName], Req) ->
    {ok, Response} = state_store:fetch(["services", Host, ServiceName]),
    reply(Response, Req);
handle_rest_api_call([<<"hosts">>, Host, <<"services">>], Req) ->
    {ok, Response} = state_store:fetch(["hosts", Host, "services"]),
    reply(Response, Req);
handle_rest_api_call([<<"targets">>, Target, <<"hosts">>], Req) ->
    {ok, Response} = state_store:fetch(["targets", Target, "hosts"]),
    reply(Response, Req);
handle_rest_api_call([<<"targets">>, Target, <<"full">>], Req) ->
    {ok, Hosts} = state_store:fetch(["targets", Target, "hosts"]),
    Responses = lists:map(
                  fun (Host) ->
                          {ok, ServicesString} = state_store:fetch([<<"hosts">>, Host, <<"services">>]),
                          Services = binary:split(ServicesString, <<"\n">>, [global]),
                          ServiceStates = lists:map(
                                            fun (Service) ->
                                                    {ok, State} = state_store:fetch([<<"services">>, Host, Service]),
                                                    [{<<"name">>, Service}, {<<"state">>, State}]
                                            end,
                                            Services
                                           ),
                          {ok, ArtefactsString} = state_store:fetch([<<"hosts">>, Host, <<"artefacts">>]),
                          ArtefactsList = binary:split(ArtefactsString, <<"\n">>, [global]),
                          Artefacts = lists:map(
                                        fun (ArtefactItem) ->
                                                [Name, Version] = binary:split(ArtefactItem, <<" ">>),
                                                [{<<"name">>, Name}, {<<"version">>, Version}]
                                        end,
                                        ArtefactsList
                                       ),
                          [{<<"host">>, Host}, {<<"services">>, ServiceStates}, {<<"artefacts">>, Artefacts}]
                  end,
                  binary:split(Hosts, <<"\n">>, [global])),
    reply(jsx:prettify(jsx:encode(Responses)), Req);
handle_rest_api_call(Path, Req) ->
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

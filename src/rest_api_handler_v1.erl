%% @private
-module(rest_api_handler_v1).
-export([handle_rest_api_call/2]).

wait_for_response_and_reply(Where, Req) ->
    receive
        {ok, Response} ->
            reply(Response, Req);
        _ ->
            io:format("unknown reply from state_store ~p~n", [Where]),
            reply("unknown reply", Req)
    after
        2000 ->
            io:format("timeout while fetching ~p~n", [Where]),
            reply("timeout", Req)
    end.

wait_for_response() ->
    receive
        {ok, Response} ->
            {ok, Response};
        Else ->
            {error, unknown_response, Else}
    after
        2000 ->
            {error, timeout}
    end.

handle_rest_api_call([], Req) ->
    reply(<<"ok">>, Req);
handle_rest_api_call([<<"services">>, Host, ServiceName]=Where, Req) ->
    state_store:fetch(["services", Host, ServiceName], self()),
    wait_for_response_and_reply(Where, Req);
handle_rest_api_call([<<"hosts">>, Host, <<"services">>]=Where, Req) ->
    state_store:fetch(["hosts", Host, "services"], self()),
    wait_for_response_and_reply(Where, Req);
handle_rest_api_call([<<"targets">>, Target, <<"hosts">>]=Where, Req) ->
    state_store:fetch(["targets", Target, "hosts"], self()),
    wait_for_response_and_reply(Where, Req);
handle_rest_api_call([<<"targets">>, Target, <<"full">>], Req) ->
    state_store:fetch(["targets", Target, "hosts"], self()),
    {ok, Hosts} = wait_for_response(),
    Responses = lists:map(
                  fun (Host) ->
                          state_store:fetch([<<"hosts">>, Host, <<"services">>], self()),
                          {ok, ServicesString} = wait_for_response(),
                          Services = string:tokens(ServicesString, "\n"),
                          ServiceStates = lists:map(
                                            fun (Service) ->
                                                    state_store:fetch([<<"services">>, Host, Service], self()),
                                                    {ok, State} = wait_for_response(),
                                                    [{<<"name">>, list_to_binary(Service)}, {<<"state">>, list_to_binary(State)}]
                                            end,
                                            Services
                                           ),
                          state_store:fetch([<<"hosts">>, Host, <<"artefacts">>], self()),
                          {ok, ArtefactsString} = wait_for_response(),
                          ArtefactsList = string:tokens(ArtefactsString, "\n"),
                          Artefacts = lists:map(
                                        fun (ArtefactItem) ->
                                                [Name, Version] = string:tokens(ArtefactItem, " "),
                                                [{<<"name">>, list_to_binary(Name)}, {<<"version">>, list_to_binary(Version)}]
                                        end,
                                        ArtefactsList
                                       ),
                          [{<<"host">>, list_to_binary(Host)}, {<<"services">>, ServiceStates}, {<<"artefacts">>, Artefacts}]
                  end,
                  string:tokens(Hosts, "\n")),
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

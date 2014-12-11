%% @private
-module(rest_api_handler_v1).
-export([handle_rest_api_call/2]).

wait_for_response_and_reply(Where, Req) ->
    receive
        {ok, Response} ->
            reply(200, Response, Req);
        _ ->
            io:format("unknown reply from state_store ~p~n", [Where]),
            reply(404, "unknown reply", Req)
    after
        2000 ->
            io:format("timeout while fetching ~p~n", [Where]),
            reply(404, "timeout", Req)
    end.

wait_for_response() ->
    receive
        {ok, _} = Response ->
            Response;
        {error, _} = Error ->
            Error;
        Everything ->
            io:format("Worse things happen at sea...~n~p~n", [Everything]),
            {error, Everything}
    after
        2000 ->
            {error, timeout}
    end.

handle_rest_api_call([], Req) ->
    reply(200, <<"ok">>, Req);
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
    fetch_full_target(wait_for_response(), Req);
handle_rest_api_call(Path, Req) ->
    cowboy_req:reply(
      404,
      [ {<<"content-type">>, <<"text/plain">>} ],
      list_to_binary(io_lib:format("no status found for ~p", [Path])),
      Req).

fetch_full_target({ok, Hosts}, Req) ->
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
    reply(200, jsx:prettify(jsx:encode(Responses)), Req);
fetch_full_target({error, Error}, Req) ->
    Responses = list_to_binary(Error),
    reply(404, jsx:prettify(jsx:encode(Responses)), Req).

reply(Status, Response, Req) ->
    cowboy_req:reply(
      Status,
      [ {<<"content-type">>, <<"text/plain">>} ],
      Response,
      Req).

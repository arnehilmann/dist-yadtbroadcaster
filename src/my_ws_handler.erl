%% @private
-module(my_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).


-define(SUBPROTHEADER,<<"sec-websocket-protocol">>).
-define(WSMSGPACK,<<"wamp.2.msgpack">>).
-define(WSJSON,<<"wamp.2.json">>).
-define(WSMSGPACK_BATCHED,<<"wamp.2.msgpack.batched">>).
-define(WSJSON_BATCHED,<<"wamp.2.json.batched">>).

-define(RIAK_URL,<<"http://localhost:8098/types/yadt/buckets/service/keys/">>).

-record(state,{
          enc = undefined,
          router = undefined,
          buffer = <<"">>
         }).

init({Transport, http}, _Req, _Opts) when Transport == tcp; Transport == ssl ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    % need to check for the wamp.2.json or wamp.2.msgpack
    {ok, Protocols, Req1} = cowboy_req:parse_header(?SUBPROTHEADER, Req),
    case lists:nth(1,Protocols) of
        ?WSMSGPACK ->
            Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSMSGPACK,Req1),
            {ok,Req2,#state{enc=msgpack}};
        ?WSMSGPACK_BATCHED ->
            Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSMSGPACK_BATCHED,Req1),
            {ok,Req2,#state{enc=msgpack_batched}};
        ?WSJSON ->
            Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSJSON,Req1),
            {ok,Req2,#state{enc=json}};
        ?WSJSON_BATCHED ->
            Req2  = cowboy_req:set_resp_header(?SUBPROTHEADER,?WSJSON_BATCHED,Req1),
            {ok,Req2,#state{enc=json_batched}};
        _ ->
            {shutdown,Req1}
    end.

websocket_handle({text, Data}, Req, #state{enc=json}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle({text, Data}, Req, #state{enc=json_batched}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle({binary, Data}, Req, #state{enc=msgpack}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle({binary, Data}, Req, #state{enc=msgpack_batched}=State) ->
  {ok,NewState} = handle_wamp(Data,State),
  {ok,Req,NewState};
websocket_handle(Data, Req, State) ->
  io:format("error: unsupported Data ~p with encoding ~p~n",[Data,State#state.enc]),
  {ok, Req, State}.

websocket_info({erwa,shutdown}, Req, State) ->
    {shutdown,Req,State};
websocket_info({erwa,Msg}, Req, #state{enc=Enc}=State) when is_tuple(Msg)->
    Rpl = erwa_protocol:serialize(Msg,Enc),
    Reply =
    case Enc of
        json -> {text,Rpl};
        msgpack -> {binary,Rpl}
    end,
    {reply,Reply,Req,State};
websocket_info(_Data, Req, State) ->
    {ok,Req,State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.


handle_wamp(Data,#state{buffer=Buffer, enc=Enc, router=Router}=State) ->
    {Messages,NewBuffer} = erwa_protocol:deserialize(<<Buffer/binary, Data/binary>>,Enc),
    {ok,NewRouter} = erwa_protocol:forward_messages(Messages,Router),
    ok = inspect_messages(Messages),
    {ok,State#state{router=NewRouter,buffer=NewBuffer}}.

inspect_messages([]) ->
    ok;
inspect_messages([Message|Rest]) ->
    inspect_message(Message),
    inspect_messages(Rest).

inspect_message({publish, _, _, _Topic, Payload, _}) ->
    %io:format("publish_event found on ~p with the following payload:~n~p~n", [Topic, Payload]),
    deep_inspect(Payload);
inspect_message(_Message) ->
    %io:format("inspecting message:~n~p~n", [Message]),
    ok.

store_service_state(Service, State) ->
    Url = binary_to_list(?RIAK_URL) ++ binary_to_list(Service),
    io:format("url: ~s -> ~s~n", [Url, State]),
    {ok, StatusCode, _RespHeaders, _ClientRef} = hackney:put(Url, [{<<"Content-Type">>, <<"text/plain">>}], State, []),
    io:format("status of store: ~p~n", [StatusCode]),
    ok.

%fetch_service_state(Service) ->
%    Url = binary_to_list(?RIAK_URL) ++ binary_to_list(Service),
%    io:format("url?: ~s~n", [Url]),
%    {ok, StatusCode, RespHeaders, ClientRef} = hackney:get(Url, [{<<"Content-Type">>, <<"text/plain">>}], <<>>, []),
%    io:format("status: ~p~n", [StatusCode]),
%    io:format("headers: ~p~n", [RespHeaders]),
%    Body = hackney:body(ClientRef),
%    io:format("body: ~p~n", [Body]),
%    ok.

deep_inspect([]) ->
    ok;
deep_inspect([{<<"payload">>, Payload}, {<<"type">>, <<"event">>}, {<<"id">>, <<"service-change">>}, _, _]) ->
    [{Service, State}] = Payload,
    ok = store_service_state(Service, State),
    io:format("~s is ~s~n", [Service, State]);
deep_inspect([{<<"payload">>, _Payload}, {<<"type">>, <<"event">>}, {<<"id">>, <<"full-update">>}, _, {<<"target">>, Topic}]) ->
    io:format("full update received of ~s~n", [Topic]);
deep_inspect([{<<"payload">>, _}, _, _, _, _]) ->
    ok;
deep_inspect([Payload|Rest]) ->
    deep_inspect(Payload),
    deep_inspect(Rest);
deep_inspect(_) ->
    ok.

-ifdef(TEST).




-endif.

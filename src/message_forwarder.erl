-module(message_forwarder).

%% API.
-export([start_listener/0]).
-export([listen_for_forwards/0]).


start_listener() ->
    ForwardListener = erlang:spawn_link(?MODULE, listen_for_forwards, []),
    register(forwards, ForwardListener),
    ok.

listen_for_forwards() ->
    receive
        {From, Realm, Data} ->
            io:format("received data from ~p on realm ~p~n", [From, Realm]),
            forward_message(Data, Realm)
    end,
    listen_for_forwards().

forward_message(Message, Realm) when is_bitstring(Realm) ->
    case erwa_realms:get_router(Realm) of
        {ok,Pid} ->
            forward_message(Message,Pid);
        {error,not_found} ->
            self() ! {erwa,{abort,[{}],no_such_realm}},
            self() ! {erwa, shutdown},
            {error,undefined}
    end;
forward_message(Message, Router) when is_pid(Router) ->
    Router ! {forwarded, Message},
    ok;
forward_message(AnyData, AnyObject) ->
    io:format("cannot handle realm/router ~p~ndropping data ~p~n", [AnyObject, AnyData]),
    ok.

-module(client).

-export([start/0]).

start() ->
    {ok, Host} = inet:gethostname(),
    {"APP-0", {_,_,{IP}}, _} = inet:getifaddrs(),
    {ok, Socket} = ets:new(socket, [named_table]),
    {ok, Ref} = gen_tcp:connect(IP, 4242, [binary, {active,true}]),
    ets:insert(socket, {Ref, IP, Host}),
    loop(Socket, Host).

loop(Socket, Host) ->
    receive
        {Msg, From} ->
            io:format("Received message ~p from ~p~n", [Msg, From]),
            io:format("Sending back an acknowledgement~n", []),
            gen_tcp:send(Socket, Host, Ref),
            loop(Socket, Host)
    end.
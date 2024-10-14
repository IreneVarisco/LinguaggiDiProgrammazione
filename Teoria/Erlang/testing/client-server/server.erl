-module(server).

-export([start/0]).

start() ->
    {ok, Ref} = gen_tcp:listen(4242, [binary, {active, true}, {reuseaddr, true}]),
    ets:new(clients, [named_table]),
    loop(Ref).

loop(Ref) ->
    receive
        {tcp, Socket, Host} ->
            ets:insert(clients, {Socket, Host}),
            loop(Ref)
    after 0 ->
        io:format("Broadcasting message ~p to all clients~n", ["Hello!"]),
        [gen_tcp:send(Client, "Hello!") || {_, Client} <- ets:all(clients)],
        loop(Ref)
    end.

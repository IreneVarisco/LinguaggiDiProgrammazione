-module(es3).
-export([start/1, to_slave/2, master/1, master_loop/1, slave/1]).

start(N) ->
    case whereis(master) of
        undefined -> ok;
        Pid -> unregister(master), exit(Pid, kill)
    end,
    register(master, spawn(fun() -> master(N) end)),
    true.

to_slave(Message, N) ->
    master ! {send_message, Message, N},
    {Message, N}.

master(N) ->
    Slaves = [{I, spawn_link(fun() -> slave(I) end)} || I <- lists:seq(1, N)],
    process_flag(trap_exit, true),
    master_loop(Slaves).

master_loop(Slaves) ->
    receive
        {send_message, Message, N} ->
            {_, Pid} = lists:nth(N, Slaves),
            Pid ! {Message, N},
            master_loop(Slaves);
        {'EXIT', Pid, _Reason} ->
            io:format("master restarting dead slave~p~n", [Pid]),
            {Index, _} = lists:keyfind(Pid, 2, Slaves),
            NewSlave = spawn_link(fun() -> slave(Index) end),
            NewSlaves = lists:keystore(Index, 1, Slaves, {Index, NewSlave}),
            master_loop(NewSlaves)
    end.

slave(N) ->
    receive
        {Message, N} ->
            io:format("Slave ~p got message ~p~n", [N, Message]),
            case Message of
                die -> exit(die);
                _ -> slave(N)
            end
    end.
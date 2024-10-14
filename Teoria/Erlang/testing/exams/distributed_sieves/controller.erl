-module(controller).
-export([start/1]).

start(N) -> register(controller, spawn(fun() -> init_ring(N) end)).
init_ring(N) ->
   loop(N, connect([spawn_link(sieve, init, [X]) || X <- lists:seq(2,N),
                                                    (length([Y||Y<-lists:seq(2, trunc(math:sqrt(X))), ((X rem Y) == 0)])==0)])).
connect(L=[H|TL]) -> connect(H, L, TL++[H]).
connect(Pid, [Pid1|[]], [Pid2|[]]) ->
   Pid1! {who, self()},
   receive
      {who, Max} -> Pid1 !{Pid, Pid2}, loop(Max, Pid)
   end;
connect(Pid, [Pid1|TL1], [Pid2 |TL2]) ->
   Pid1 !{Pid, Pid2},
   connect(Pid, TL1, TL2).

loop(Max, Head) ->
   receive
      {new, N, From} ->
         io:format("You asked for: ~p~n", [N]), RootN = trunc(math:sqrt(N)),
         if
            (RootN < Max) -> Head! {new, N},
                             receive {res, V} ->
                                        From!{result,
                                              lists:flatten(io_lib:format("is ~p prime? ~p",[N,V]))}
                             end,
                             loop(Max, Head);
            true ->
               From!{result,lists:flatten(
                              io_lib:format("~p is uncheckable, too big value.",[N]))},
               loop(Max, Head) end;
      {quit, From} ->
         io:format("I'm closing ...~n"), From ! {result, "The service is closed!!!"}
   end.

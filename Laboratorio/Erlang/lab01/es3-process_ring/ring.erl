-module(ring).

-export([start/3]).

start(M, N, Msg) ->
  create(N),
  send_messages(M, {1, Msg}),
  timer:sleep(10),
  fst_node ! stop,
  ok.

send_messages(0, _) -> ok;
send_messages(M, {Idx, Msg}) ->
  fst_node ! {0, Idx, Msg},
  send_messages(M - 1, {Idx + 1, Msg}).

create(N) ->
  Pid = spawn(fun() -> ring_node(fst_node) end),
  io:format("*** [main] created node ~p with pid ~p~n", [N, Pid]),
  create(Pid, N - 1).

create(Next, 1) ->
  register(fst_node, spawn(fun() -> ring_node(fst_node) end)),
  io:format("*** [main] created node 1 (fst_node) with pid ~p linked to node 2 with pid ~p~n", [whereis(fst_node), Next]);
create(Next, N) ->
  Pid = spawn(fun() -> ring_node(Next) end),
  io:format("*** [main] created node ~p with pid ~p linked to node ~p with pid ~p~n", [N, Pid, N + 1, Next]),
  create(Pid, N - 1).


ring_node(Next) ->
  receive
      stop ->
         debug_print("stop", Next),
         catch Next ! stop,
         debug_print("stopping . . ."),
         exit(stop);
    {ForwardedCount, Idx, Msg} ->
      debug_print(ForwardedCount, Idx, Msg, Next),
      catch Next ! {ForwardedCount +  1, Idx, Msg},
      ring_node(Next)
  end.

  
debug_print(Cnt, Idx, Msg, To) ->
  io:format("[From: ~p] -> [To: ~p] - ~p:~p (forwarded ~p times)~n", [self(), To, Idx, Msg, Cnt]).
debug_print(Msg, To) -> io:format("[From: ~p] -> [To: ~p] ~p~n", [self(), To, Msg]).
debug_print(Msg) -> io:format("[Pid: ~p] -> [To: ~p] -p~n", [self(), Msg]).

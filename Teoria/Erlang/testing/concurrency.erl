-module(concurrency).

-export([start/2, loop/2]).

start(Num, Id) -> spawn(concurrency, loop, [Num, Id]).

loop(0, Id) -> io:format("[pid: ~p, id: ~p] - end~n", [self(), Id]);
loop(Num, Id) ->
  io:format("[pid: ~p, id: ~p] - ~p~n", [self(), Id, Num]),
  loop(Num - 1, Id).

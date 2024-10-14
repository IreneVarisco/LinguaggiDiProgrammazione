-module(server).
-export([init/0]).

init() ->
  group_leader(whereis(user), self()),
  io:format("Server started~n"),
  loop(1, 1, [], []).

loop(Index1, Index2, Acc1, Acc2) ->
  receive
    {mm1, { Index1, Original, Size, Value } = What} when Index1 == Size, Index2 == Size+1 ->
      io:format("Received ~p from mm1~n", [What]),
      io:format("The reverse of ~p is ~p~n", [Original, concat(length(Original), [Value|Acc1], Acc2)]),
      loop(1, 1, [], []);
    {mm2, { Index2, Original, Size, Value } = What} when Index2 == Size, Index1 == Size+1 ->
      io:format("Received ~p from mm2~n", [What]),
      io:format("The reverse of ~p is ~p~n", [Original, concat(length(Original), Acc1, [Value|Acc2])]),
      loop(1, 1, [], []);
    {mm1, { Index1, _, _, Value } = What} ->
      io:format("Received ~p from mm1~n", [What]),
      loop(Index1+1, Index2, [Value|Acc1], Acc2);
    {mm2, { Index2, _, _, Value } = What} ->
      io:format("Received ~p from mm2~n", [What]),
      loop(Index1, Index2+1, Acc1, [Value|Acc2])
  end.

concat(N, X, Y) when N rem 2 == 0 -> Y++X;
concat(_, [_|T], Y)               -> Y++T.

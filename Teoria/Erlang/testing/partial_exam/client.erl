-module(client).
-export([start/0, close/0, do_reverse/1]).

start() ->
  group_leader(whereis(user), self()),
  {ok, HostName} = inet:gethostname(),
  Server = spawn_link(list_to_atom("server@"++HostName), server, init, []),
  MM1 = spawn_link(list_to_atom("mm1@"++HostName), mm, init, [mm1, Server]),
  MM2 = spawn_link(list_to_atom("mm2@"++HostName), mm, init, [mm2, Server]),
  register(client, spawn_link(fun() -> loop(MM1, MM2) end)),
  io:format("Server started on ~p~n", [HostName]),
  io:format("Nodes :- ~p~n", [nodes()]).

do_reverse(L) ->
  if
    length(L) rem 2 == 0 ->
      Size = trunc(length(L)/2),
      client ! {L, lists:sublist(L, Size), lists:sublist(L, Size+1, Size)};
    true                 ->
      Size = trunc(length(L)/2) + 1,
      client ! {L, lists:sublist(L, Size), lists:sublist(L, Size, Size)}
  end.

loop(MM1, MM2) ->
  receive
    {Original, FirstHalf, SecondHalf} -> forward(MM1, MM2, 1, Original, FirstHalf, SecondHalf, length(FirstHalf)), loop(MM1, MM2)
  end.

forward(MM1, MM2, Index, Original, [X], [Y], Size)         ->
  MM1 ! {Index, Original, Size, X},
  MM2 ! {Index, Original, Size, Y};
forward(MM1, MM2, Index, Original, [H1|T1], [H2|T2], Size) ->
  MM1 ! {Index, Original, Size, H1},
  MM2 ! {Index, Original, Size, H2},
  forward(MM1, MM2, Index+1, Original, T1, T2, Size).

close() ->
  io:format("closing~n"),
  exit(closed).

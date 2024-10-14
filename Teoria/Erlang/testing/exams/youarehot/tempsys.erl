-module(tempsys).
-export([startsys/0]).

-define(CONCAT_ATOM(A, B), list_to_atom(lists:concat([A,B]))).

fromC(X)  -> X.
fromDe(X) -> 100 - X * 2 / 3.
fromF(X)  -> ( X - 32 ) * 5 / 9.
fromK(X)  -> X - 273.15.
fromN(X)  -> X * 100/33.
fromR(X)  -> ( X - 491.67 ) * 5 / 9.
fromRe(X) -> X * 5 / 4.
fromRo(X) -> ( X - 7.5 ) * 40 / 21.

toC(X)    -> X.
toDe(X)   -> ( 100 - X ) * 3 / 2.
toF(X)    -> X * 9 / 5 + 32.
toK(X)    -> X + 273.15.
toN(X)    -> X * 33 / 100.
toR(X)    -> X * 9 / 5 + 491.67.
toRe(X)   -> X * 4 / 5.
toRo(X)   -> X * 21 / 40 + 7.5.

regT(T={K,V}) -> register(?CONCAT_ATOM(from, K), spawn(fun() -> loop(V) end)), T.
regTto(T={K,V}) -> register(?CONCAT_ATOM(to, K), spawn(fun() -> loopto(V) end)), T.


%this is the second line of actors
loopto(F) ->
  receive
    {client, C, stub, From, celsius, X} ->
      From ! {client, C, result, F(X)},
      io:format("~p", [X]),
      loopto(F);
    Other -> io:format("LoopTo Error: ~p~n", [Other])
  end.

%this is the first line of actors
loop(F) ->
  receive
    {who, From, to, T, val, X} ->
      ?CONCAT_ATOM(to,T) ! {client, From, stub, self(), celsius, F(X)},
      loop(F);

    {client, C, result, X} ->
        C ! {result, X},
        loop(F);

    Other -> io:format("Loop Error: ~p~n", [Other])
  end.


startsys() ->
  FromT = [{'C', fun fromC/1},
           {'De', fun fromDe/1},
           {'F', fun fromF/1},
           {'K', fun fromK/1},
           {'N', fun fromN/1},
           {'R', fun fromR/1},
           {'Re', fun fromRe/1},
           {'Ro', fun fromRo/1}],
  ToT = [{'C', fun toC/1},
         {'De', fun toDe/1},
         {'F', fun toF/1},
         {'K', fun toK/1},
         {'N', fun toN/1},
         {'R', fun toR/1},
         {'Re', fun toRe/1},
         {'Ro', fun toRo/1}],
  lists:map(fun regT/1, FromT),
  lists:map(fun regTto/1, ToT).

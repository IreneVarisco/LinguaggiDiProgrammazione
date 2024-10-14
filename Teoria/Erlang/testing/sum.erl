-module(sum).
-export([sum/1]).

sum([]) -> 0;
sum([H|T]) -> H + sum(T).


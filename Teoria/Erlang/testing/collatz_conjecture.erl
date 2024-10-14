-module(collatz_conjecture).

-export([steps/2]).

steps(N, Counter) when N =:= 1 -> Counter;
steps(N, Counter) when
             (N rem 2) =:= 0 -> steps((N div 2), Counter + 1);
steps(N, Counter) when
             (N rem 2) > 0 -> steps((N div 3) + 1, Counter + 1).


-module(primes).
-export([untill/1]).


untill(N) when N < 2 -> [];
untill(N)            -> [X || X <- lists:seq(2, N), is_prime(X)].


is_prime(N) -> length([X || X <- lists:seq(2, trunc(math:sqrt(N))), N rem X == 0]) == 0.

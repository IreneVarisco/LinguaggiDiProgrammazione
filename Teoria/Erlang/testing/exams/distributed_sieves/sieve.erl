-module(sieve).
-export([init/1]).

init(N) -> receive
              {who, From} -> From ! {who, N}, init(N);
              {Gate, To} -> loop(Gate, To, N) end.

loop(Gate, To, N) -> receive
                        {new, N1} -> Gate ! {pass, N1}, loop(Gate, To, N);
                        {pass, N1} ->
                           RootN1 = trunc(math:sqrt(N1)),
                           if
                              (N > RootN1) -> Gate ! {res, true},
                                              loop(Gate, To, N);
                              ((N1 rem N) == 0) -> Gate ! {res, false},
                                                   loop(Gate, To, N);
                              true -> To ! {pass, N1}, loop(Gate, To, N)
                           end;

                        {res, V} -> controller ! {res, V}, loop(Gate, To, N);
                        Other -> io:format("unrecognized message:- ~p~n", [Other]), loop(Gate, To, N)
                     end.

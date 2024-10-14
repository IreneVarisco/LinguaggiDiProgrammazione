-module(combinator).
-export([start/2]).

start(N, M) ->
  register(entrypoint, spawn(fun() -> init_slaves(N,M) end)).

init_slaves(N, M) ->
  [spawn_link(generator, init, [P, N, M]) || P <- lists:seq(1,N)],
  collect(N,M,1,1,trunc(math:pow(M, N))).


collect(N, M, P, Seq, Max) ->
  receive
    {seq, Seq, val, Value, pos, P} when (Seq==Max) and ((P+1)>N) ->
      io:format("~p~n", [Value]), exit(entrypoint), unregister(entrypoint);

    {seq, Seq, val, Value, pos, P} when ((P+1)>N) ->
      io:format("~p~n", [Value]), collect(N,M,1,Seq+1,Max);

    {seq, Seq, val, Value, pos, P} ->
      io:format("~p, ", [Value]), collect(N,M,P+1,Seq,Max)
  end.



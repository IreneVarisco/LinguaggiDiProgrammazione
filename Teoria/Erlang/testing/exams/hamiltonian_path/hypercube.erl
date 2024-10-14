-module(hypercube).
-export([create/0,hamilton/2,gray/1]).


create() ->
  PIDs = [{G, spawn(node, start, [G])} || G <- gray(4)],
  lists:foreach(
    fun({G,Ns}) ->
        {_,P} = lists:keyfind(G, 1, PIDs),
        P! {neighbors, pair_pids(PIDs, Ns), src, self()}
    end, grayneighbors(gray(4))),
  {_,P} =lists:keyfind("0000", 1, PIDs), register(zero, P).


hamilton(M, [HG|TG]) ->
  zero ! {msg, {src, HG, msg, M}, path, TG},
  receive
    Other -> io:format("~p~n", [Other]) end.

gray(0) -> [""];
gray(N) -> G = gray(N-1), [ "0"++L || L <- G ]++[ "1"++L || L <- lists:reverse(G) ].

strxor([],[]) -> [];
strxor([H1|T1],[H2|T2]) -> [(H1 bxor H2)+$0 |strxor(T1,T2)].

neighbors(Lab) ->
  [strxor(X,N) || {X,N} <- lists:zip([X || X <- [Lab], _ <-lists:seq(0,3)],
  ["1000", "0100", "0010", "0001"])] .

grayneighbors([]) -> [];
grayneighbors([H|T]) -> [{H,neighbors(H)}]++grayneighbors(T).

pair_pids(_, []) -> [];
pair_pids(PIDs, [H|T]) -> {H,P} = lists:keyfind(H,1,PIDs),[{H,P}]++pair_pids(PIDs, T).

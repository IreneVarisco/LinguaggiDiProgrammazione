-module(joseph).
-export([joseph/2]).

joseph(N,K) ->
  PIDS = [spawn(hebrew, start, [G, N, K]) || G <- lists:seq(1,N)],
  [P! {neighbor, Next, master, self()} ||
   {P, Next} <- lists:zip(PIDS, tl(PIDS)++[hd(PIDS)])],
  hd(PIDS) ! {msg, "Who will survive?",
              from,
              lists:last(PIDS), stepn, 1, stepk, 1},
  receive
    {msg, "I'm the survivor!", from, _, label, L} ->
      io:format("In a circle of ~p people, killing number ~p~n
                Joseph is the Hebrew in position ~p~n", [N, K, L]);
    Other -> io:format("### error ~p~n", [Other])
  end.

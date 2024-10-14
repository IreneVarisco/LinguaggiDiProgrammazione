-module(hebrew).
-export([start/3]).
start(Label, N, K) -> receive
                         {neighbor, PID, master, M} -> loop(Label, PID, M, N, K);
                         Other -> io:format("### error ~p~n", [Other]) end.

loop(Label,Next,Master,N,K) ->
   receive
      % Your neighbor is died! Long live to the new neightbor!
      {newn, NewNext} -> loop(Label, NewNext, Master, N, K);
      % You are the last standing! Communicate it to the master!
      {msg, _, from, _, stepn, N, stepk, 1} -> Master ! {msg, "I'm the survivor!", from, self(), label, Label};
      % This is not your lucky day! You drawn the shortest match.
      {msg, MSG, from, Prev, stepn, Sn, stepk, K} ->
         Prev!{newn,Next}, Next!{msg,MSG,from,Prev,stepn,Sn+1,stepk,1}; % Phwee!
      {msg, MSG, from, _, stepn, Sn, stepk, Sk} ->
         Next! {msg, MSG, from, self(), stepn, Sn, stepk, Sk+1}, loop(Label, Next, Master, N, K)
   end.

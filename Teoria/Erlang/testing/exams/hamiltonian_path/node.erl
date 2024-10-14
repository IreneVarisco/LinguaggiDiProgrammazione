-module(node).
-export([start/1]).

start(G) ->
  io:format("The process labeled ~p just started~n", [G]),
  receive
    {neighbors, PIDs, src, S} -> loop(G, PIDs, S);
    Other -> io:format("### error ~p~n", [Other])
  end.


loop(G,Ns,S) ->
  receive
    {msg, M, path, [HP|[]]} ->
      S ! {msg, {src, HP, msg, M}}, loop(G,Ns,S); {msg, M, path, [HP|TP]} ->
      case lists:keyfind(HP, 1, Ns) of
        {HP, P} -> P! {msg, {src, HP, msg, M}, path, TP},
                   loop(G,Ns,S);
        Other -> io:format("~p~n", [Other])
      end
  end.

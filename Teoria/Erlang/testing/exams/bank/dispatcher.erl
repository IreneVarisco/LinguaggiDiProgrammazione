-module(dispatcher).
-export([rpc/1]).


start(Node) ->
  receive
    {Node} -> io:format("I'm Node ~p~n", [Node]);
    {MM, MSG} -> io:format("I'm MM~p and I dealt with MSH #~p~n", [MM, MSG])
  end.

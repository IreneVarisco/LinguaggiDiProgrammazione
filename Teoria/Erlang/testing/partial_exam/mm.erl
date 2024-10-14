-module(mm).
-export([init/2]).

init(Me, Server) ->
  group_leader(whereis(user), self()),
  io:format("~p started~n", [Me]),
  loop(Me, Server).

loop(Me, Server) ->
  receive
    Any -> io:format("[~p] forwarding ~p~n", [Me, Any]),
           Server ! {Me, Any},
               loop(Me, Server)
  end.

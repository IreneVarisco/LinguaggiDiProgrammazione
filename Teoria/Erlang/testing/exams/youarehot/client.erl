-module(client).
-export([convert/5]).

-define(CONCAT_ATOM(A, B), list_to_atom(lists:concat([A,B]))).

% c(tempsys).
% c(client).
% tempsys:startsys().
% client:convert(from, 'Re', to, 'De', 25).
% client:convert(from, 'K', to, 'N', -25).
% Conv = fun(X) -> client:convert(from, X, to, 'C', 32) end.
% lists:map(Conv, ['C','De', 'F', 'K', 'N', 'R', 'Re', 'Ro']).

convert(from, S, to, T, Value) ->
   ?CONCAT_ATOM(from, S) ! {who, self(), to, T, val, Value},
   receive
      {result, V} -> io:format("~p°~s are equivalent to ~p°~s~n", [Value, S, V, T]);

      Other -> io:format("~p~n", [Other])
   end.

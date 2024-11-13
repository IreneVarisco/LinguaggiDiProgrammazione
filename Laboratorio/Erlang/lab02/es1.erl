-module(es1).
-export([squared_int/1, intersect/2, symmetric_difference/2]).



squared_int([]) -> [];
squared_int([Testa|Coda]) ->
    case is_integer(Testa) of
        true -> [Testa * Testa | squared_int(Coda)];
        false -> squared_int(Coda)
    end.

intersect([], _) -> [];
intersect(_, []) -> [];
intersect([Testa|Coda], Lista) ->
    case lists:member(Testa, Lista) of
        true -> [Testa | intersect(Coda, Lista)];
        false -> intersect(Coda, Lista)
    end.

symmetric_difference([], Lista) -> Lista;
symmetric_difference(Lista, []) -> Lista;
symmetric_difference([Testa|Coda], Lista) ->
    case lists:member(Testa, Lista) of
        false -> [Testa | symmetric_difference(Coda, Lista)];
        true -> symmetric_difference(Coda, lists:delete(Testa, Lista))
    end.

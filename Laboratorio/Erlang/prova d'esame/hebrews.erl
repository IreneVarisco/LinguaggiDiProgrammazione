-module(hebrews).
-export([eliminate/2]).

eliminate(NumPeople, LastPosition) ->
    eliminate(NumPeople, LastPosition, 1, 1, []).

eliminate(NumPeople, LastPosition, CurrPos, CurrNum, Survivors) ->
    case CurrPos of
        LastPosition ->
            lists:reverse([CurrNum | Survivors]);
        NumPeople ->
            eliminate(NumPeople, 1, 1, CurrNum + 1, [CurrNum | Survivors]);
        _ ->
            eliminate(NumPeople, LastPosition, CurrPos + 1, CurrNum + 1, Survivors)
    end.
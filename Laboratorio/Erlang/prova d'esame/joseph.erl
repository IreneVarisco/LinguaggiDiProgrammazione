-module(joseph).
-export([find_position/1, start/0]).


find_position(NumPeople) ->
    find_position(NumPeople, 1, 1).

find_position(NumPeople, CurrPos, Survivor) ->
    case CurrPos of
        NumPeople -> 
            Survivor;
        _ ->
            NextPos = ((CurrPos + 2) rem NumPeople) + 1,
            NextSurvivor = CurrPos + 1,
            find_position(NumPeople, NextPos, NextSurvivor)
    end.


start() ->  
    NumPeople = 40,
    LastSurvivor = find_position(NumPeople),
    EliminatedHebrews = hebrews:eliminate(NumPeople, LastSurvivor),
    io:format("L'ultimo sopravvissuto è l'Ebreo in posizione ~B~n", [LastSurvivor]),
    io:format("Gli Ebrei eliminati sono: ~w~n", [EliminatedHebrews]).
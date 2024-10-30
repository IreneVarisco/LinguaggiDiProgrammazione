-module(es1).
-export([is_palindrome/1, invert/1, remove_spaces/1, is_an_anagram/2]).

invert(List) -> invert(List, []).
invert([], Acc) -> Acc;
invert([H|T], Acc) -> invert(T, [H|Acc]).

remove_spaces([]) -> [];
remove_spaces([H|T]) when H =:= $\s -> remove_spaces(T);
remove_spaces([H|T]) when H =:= $\n -> remove_spaces(T);
remove_spaces([H|T]) when H =:= $\t -> remove_spaces(T);
remove_spaces([H|T]) -> [H | remove_spaces(T)].

is_palindrome([]) -> true;
is_palindrome(String) ->
    String1 = string:lowercase(remove_spaces(String)),
    String1 == invert(String1).

sort_string(String) -> 
    lists:sort(String).

is_an_anagram([], _) -> true;
is_an_anagram(String, Dictionary) -> 
    Sorted = string:lowercase(sort_string(remove_spaces(String))),
    lists:any(fun(DictString) -> Sorted == sort_string(DictString) end, Dictionary).


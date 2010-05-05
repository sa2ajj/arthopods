-module(utils).

-export([select/1, index/2, round_to_power_of_2/1]).

-import(random, [uniform/1]).

select(List) when is_list(List) ->
    lists:nth(uniform(length(List)), List);

select(Tuple) when is_tuple(Tuple) ->
    element(uniform(tuple_size(Tuple)), Tuple).

index(Item, List) when is_list(List) -> index(Item, List, 1).

index(Item, [ Item | _Tail ], Index) -> Index;
index(Item, [ _Head | Tail ], Index) -> index(Item, Tail, Index+1).

round_to_power_of_2(Number)
    when is_integer(Number) andalso Number >= 0 ->

    round_to_power_of_2_2(Number, 1).

round_to_power_of_2_2(Number, Result)
    when Number =< Result -> Result;

round_to_power_of_2_2(Number, Result) ->
    round_to_power_of_2_2(Number, Result bsl 1).

% vim:ts=4:sw=4:et

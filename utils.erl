-module(utils).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([index/2, round_to_power_of_2/1, sleep/1, gen_unique_id/1]).

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

sleep(T) ->
    receive
    after T
        -> ok
    end.

-spec gen_unique_id(string()) -> atom().

gen_unique_id(Prefix) ->
    list_to_atom(Prefix++erlang:ref_to_list(make_ref())).

% vim:ts=4:sw=4:et

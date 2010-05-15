-module(select).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([uniform/1]).

uniform(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List);

uniform(Tuple) when is_tuple(Tuple) ->
    element(random:uniform(tuple_size(Tuple)), Tuple).

% vim:ts=4:sw=4:et

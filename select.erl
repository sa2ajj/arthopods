-module(select).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([uniform/1, quadratic/1]).

uniform(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)), List);

uniform(Tuple) when is_tuple(Tuple) ->
    element(random:uniform(tuple_size(Tuple)), Tuple).

% gets a list of tuples {Tag, Weight}
% returns a Tag[i] with probability Weight[i]/sum(Weight[i]^2)

quadratic(List) ->
    { Total, Bounds } = calculate(List, 0, []),
    Number = random:uniform(Total),
    % io:format("total: ~p, bounds: ~p, number: ~p~n", [ Total, Bounds, Number ]),
    quadratic(Bounds, Number).

calculate([], Total, Bounds) ->
    { Total, Bounds };

calculate([ {Tag, Weight} | Tail ], Total, Bounds) ->
    calculate(Tail, Total+Weight*Weight, [ {Tag, Total} | Bounds ]).

quadratic([], _Value) ->
    something_went_wrong;

quadratic([ { Tag, Bound } | _Tail ], Value)
    when Value > Bound ->
        Tag;

quadratic([ _Head | Tail ], Value) ->
    quadratic(Tail, Value).

% vim:ts=4:sw=4:et

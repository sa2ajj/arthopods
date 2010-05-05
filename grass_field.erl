%

-module(grass_field).

-export([start/1]).

-import(utils, [round_to_power_of_2/1]).

start(MaxEdge) ->
    MaxValue = round_to_power_of_2(MaxEdge)-1,

    loop({ empty, {0, 0}, {MaxValue, MaxValue} }).

loop(GrassField) ->
    receive
        {grow, Pid, Location} ->
            NewField = grow(GrassField, Location),
            Pid ! ack_grow,
            loop(NewField);

        {cut, Pid, Location} ->
            {NewField, Result} = cut(GrassField, Location),
            Pid ! {ack_cut, Result},
            loop(NewField);

        {find, Pid, Bounds} ->
            {NewField, Amount} = find(GrassField, Bounds),
            Pid ! {ack_find, Amount},
            loop(NewField)
    end.

grow({empty, Corner0, Corner1}, Location) ->
    { leaf, Location, Corner0, Corner1 };

grow({leaf, _Leaf, _, _} = GrassField, _Leaf) ->
    GrassField;

grow({leaf, Leaf, Corner0, Corner1}, Location) ->
    Field = make_patch(Corner0, Corner1),
    grow(grow(Field, Leaf), Location);

grow({patch, {Xc, Yc} = Center, Patch1, Patch2, Patch3, Patch4}, {X, Y} = Location) ->
    case {X < Xc, Y < Yc} of
        { true, true } ->
            { patch, Center, grow(Patch1, Location), Patch2, Patch3, Patch4 };

        { false, true } ->
            { patch, Center, Patch1, grow(Patch2, Location), Patch3, Patch4 };

        { false, false } ->
            { patch, Center, Patch1, Patch2, grow(Patch3, Location), Patch4 };

        { true, false } ->
            { patch, Center, Patch1, Patch2, Patch3, grow(Patch4, Location) }
    end.

make_patch({X0, Y0}, {X1, Y1}) ->
    Xc = (X0 + X1) / 2,
    Yc = (Y0 + Y1) / 2,

    {
        patch,
        {Xc, Yc},
        { empty, {X0, Xc}, {Y0, Yc} },
        { empty, {Xc, X1}, {Y0, Yc} },
        { empty, {Xc, X1}, {Yc, Y1} },
        { empty, {X0, Xc}, {Yc, Y1} }
    }.

cut(GrassField, _Location) ->
    { GrassField, not_done }.

find(GrassField, {_Corner0, _Corner1}) ->
    { GrassField, 0 }.

% vim:ts=4:sw=4:et

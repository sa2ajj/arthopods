%

-module(grass_field).

-export([new/1, grow/2]).

-import(utils, [round_to_power_of_2/1]).

new({Width, Height}) ->
    MaxValue = round_to_power_of_2(lists:max([Width, Height]))-1,

    { empty, {0, 0}, {MaxValue, MaxValue} }.

grow({empty, Corner0, Corner1}, Location) ->
    { leaf, Location, Corner0, Corner1 };

grow({leaf, Leaf, _, _} = GrassField, Leaf) ->
    GrassField;

grow({leaf, Leaf, {X0, Y0}, {X1, Y1}}, Location) ->
    Xc = (X0 + X1) / 2,
    Yc = (Y0 + Y1) / 2,

    grow(grow({
        patch,
        {Xc, Yc},
        { empty, {X0, Xc}, {Y0, Yc} },
        { empty, {Xc, X1}, {Y0, Yc} },
        { empty, {Xc, X1}, {Yc, Y1} },
        { empty, {X0, Xc}, {Yc, Y1} }
    }, Leaf), Location);

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

% vim:ts=4:sw=4:et

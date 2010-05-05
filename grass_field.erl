%

-module(grass_field).

-export([new/1, grow/2]).

-import(utils, [round_to_power_of_2/1]).

new({Width, Height})
    when
        is_integer(Width) andalso
        is_integer(Height) andalso
        Width > 0 andalso
        Height > 0 ->

    MaxValue = round_to_power_of_2(lists:max([Width, Height]))-1,

    { empty, {0, 0}, {MaxValue, MaxValue} }.

% grow(GrassField, {X, Y} = Location)
    % TODO: add some validation for X & Y?

grow(GrassField, Location) ->
    grow2(GrassField, Location).

grow2({empty, Corner0, Corner1}, Location) ->
    { leaf, Location, Corner0, Corner1 };

grow2({leaf, Leaf, {X0, Y0} = Corner0, {X1, Y1} = Corner1}, Location) ->
    Xc = (X0 + X1) / 2,
    Yc = (Y0 + Y1) / 2,

    grow2(grow2({
        patch,
        {Xc, Yc},
        { empty, {X0, Xc}, {Y0, Yc} },
        { empty, {Xc, X1}, {Y0, Yc} },
        { empty, {Xc, X1}, {Yc, Y1} },
        { empty, {X0, Xc}, {Yc, Y1} }
    }, Leaf), Location);

grow2({patch, {Xc, Yc} = Center, Patch1, Patch2, Patch3, Patch4}, {X, Y} = Location) ->
    case {X < Xc, Y < Yc} of
        { true, true } ->
            { patch, Center, grow2(Patch1, Location), Patch2, Patch3, Patch4 };

        { false, true } ->
            { patch, Center, Patch1, grow2(Patch2, Location), Patch3, Patch4 };

        { false, false } ->
            { patch, Center, Patch1, Patch2, grow2(Patch3, Location), Patch4 };

        { true, false } ->
            { patch, Center, Patch1, Patch2, Patch3, grow2(Patch4, Location) }
    end.

% vim:ts=4:sw=4:et

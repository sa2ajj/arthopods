%

-module(grass_field).

-export([start/1]).

-import(utils, [round_to_power_of_2/1]).

start(MaxEdge) ->
    MaxValue = round_to_power_of_2(MaxEdge),

    loop({ empty, {0, 0}, {MaxValue, MaxValue} }).

%
% GrassField :=
%           % empty quadrant, defined by two corners
%               { empty, Point, Point } |
%           % position of the only leaf in quadrant, defined by two corners
%               { leaf, Point, Point, Point } |
%           % quadrant, defined by its center, divided in four smaller quadrants
%               { patch, Point, GrassField, GrassField, GrassField, GrassField }
%
% Point := { Integer, Integer }
%

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

        {find, Pid, Boundaries} ->
            NewField = find(GrassField, Boundaries, Pid),
            Pid ! {ack_find},
            loop(NewField);

        {dump, Pid} ->
            dump(GrassField),
            Pid ! ack_dump,
            loop(GrassField)
    end.

grow({empty, Corner0, Corner1}, Location) ->
    { leaf, Location, Corner0, Corner1 };

grow({leaf, _Leaf, _Corner0, _Corner1} = GrassField, _Leaf) ->
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

% Y1 +----+----+
%    | P4 | P3 |
% Yc +----+----+
%    | P1 | P2 |
% Y0 +----+----+
%    X0   Xc   X1

make_patch({X0, Y0}, {X1, Y1}) ->
    Xc = (X0 + X1) div 2,
    Yc = (Y0 + Y1) div 2,

    { patch, {Xc, Yc},
      { empty, {X0, Y0}, {Xc, Yc} },
      { empty, {Xc, Y0}, {X1, Yc} },
      { empty, {Xc, Yc}, {X1, Y1} },
      { empty, {X0, Yc}, {Xc, Y1} }}.

%% remove an item from the grass field

cut({empty, _Corner0, _Corner1} = Empty, _Location) ->
    {Empty, not_found};

cut({leaf, _Leaf, Corner0, Corner1}, _Leaf) ->
    {{empty, Corner0, Corner1}, ok};

cut({leaf, _Leaf, _Corner0, _Corner1} = Current, _Location) ->
    {Current, not_found};

cut({patch, {Xc, Yc} = Center, Patch1, Patch2, Patch3, Patch4}, {X, Y} = Location) ->
    case {X < Xc, Y < Yc} of
        { true, true } ->
            {NewPatch, Result} = cut(Patch1, Location),
            {{patch, Center, NewPatch, Patch2, Patch3, Patch4}, Result};

        { false, true } ->
            {NewPatch, Result} = cut(Patch2, Location),
            {{patch, Center, Patch1, NewPatch, Patch3, Patch4}, Result};

        { false, false } ->
            {NewPatch, Result} = cut(Patch3, Location),
            {{patch, Center, Patch1, Patch2, NewPatch, Patch4}, Result};

        { true, false } ->
            {NewPatch, Result} = cut(Patch4, Location),
            {{patch, Center, Patch1, Patch2, Patch3, NewPatch}, Result}
    end.

%% finding all leaves in the given rectangle
%% find(GrassField, Boundaries, Pid) --> NewField
%%   the function sends a message of format {leaf, Location} for each found leaf

find({leaf, {Xl, Yl} = Leaf, _Corner0, _Corner1} = Field, {{X0, Y0}, {X1, Y1}}, Pid)
    when (X0 =< Xl) and (Xl =< X1) and (Y0 =< Yl) and (Yl =< Y1) ->
        Pid ! { leaf, Leaf },
        { old, Field };

find({patch, _Center, {empty, Corner0, _}, {empty, _, _}, {empty, _, Corner1}, {empty, _, _}}, _Boundaries, _Pid) ->
    { new, {empty, Corner0, Corner1} };

find({patch, {Xc, Yc} = Center, Patch1, Patch2, Patch3, Patch4} = Field, {{Xl, Yb}, {Xr, Yt}} = Boundaries, Pid) ->
    % {X,Y}{l,r,b,t} -- left, right, bottom, top corners
    case { Xl >= Xc, Yb >= Yc, Xr < Xc, Yt < Yc } of
        % bottom left quadrant
        { _, _, true, true } ->
            case find(Patch1, Boundaries, Pid) of
                { new, NewPatch1 } ->
                    { new, {patch, Center, NewPatch1, Patch2, Patch3, Patch4}};
                _ ->
                    { old, Field }
            end;

        % bottom right quadrant
        { true, _, _, true } ->
            case find(Patch2, Boundaries, Pid) of
                { new, NewPatch2 } ->
                    { new, {patch, Center, Patch1, NewPatch2, Patch3, Patch4}};
                _ ->
                    { old, Field }
            end;

        % top right quadrant
        { true, true, _, _ } ->
            case find(Patch3, Boundaries, Pid) of
                { new, NewPatch3 } ->
                    { new, {patch, Center, Patch1, Patch2, NewPatch3, Patch4}};
                _ ->
                    { old, Field }
            end;

        % top left quadrant
        { _, true, true, _ } ->
            case find(Patch4, Boundaries, Pid) of
                { new, NewPatch4 } ->
                    { new, {patch, Center, Patch1, Patch2, Patch3, NewPatch4}};
                _ ->
                    { old, Field }
            end;

        % two bottom quadrants
        { _, _, _, true } ->
            { Modified1, NewPatch1 } = find(Patch1, Boundaries, Pid),
            { Modified2, NewPatch2 } = find(Patch2, Boundaries, Pid),

            case { Modified1, Modified2 } of
                { old, old } ->
                    { old, Field };

                _ ->
                    { new, {patch, Center, NewPatch1, NewPatch2, Patch3, Patch4} }
            end;

        % two right quadrants
        { true, _, _ , _ } ->
            { Modified2, NewPatch2 } = find(Patch2, Boundaries, Pid),
            { Modified3, NewPatch3 } = find(Patch3, Boundaries, Pid),

            case { Modified2, Modified3 } of
                { old, old } ->
                    { old, Field };

                _ ->
                    { new, {patch, Center, Patch1, NewPatch2, NewPatch3, Patch4} }
            end;

        % two upper quadrants
        { _, true, _, _ } ->
            { Modified3, NewPatch3 } = find(Patch3, Boundaries, Pid),
            { Modified4, NewPatch4 } = find(Patch4, Boundaries, Pid),

            case { Modified3, Modified4 } of
                { old, old } ->
                    { old, Field };

                _ ->
                    { new, {patch, Center, Patch1, Patch2, NewPatch3, NewPatch4} }
            end;

        % two left quadrants
        { _, _, true, _ } ->
            { Modified4, NewPatch4 } = find(Patch4, Boundaries, Pid),
            { Modified1, NewPatch1 } = find(Patch1, Boundaries, Pid),

            case { Modified4, Modified1 } of
                { old, old } ->
                    { old, Field };

                _ ->
                    { new, {patch, Center, NewPatch1, Patch2, Patch3, NewPatch4} }
            end;

        % no optimization is possible, parsing all four quadrants
        _ ->
            { Modified1, NewPatch1 } = find(Patch1, Boundaries, Pid),
            { Modified2, NewPatch2 } = find(Patch2, Boundaries, Pid),
            { Modified3, NewPatch3 } = find(Patch3, Boundaries, Pid),
            { Modified4, NewPatch4 } = find(Patch4, Boundaries, Pid),

            case { Modified1, Modified2, Modified3, Modified4 } of
                { old, old, old, old } ->
                    { new, {patch, Center, NewPatch1, NewPatch2, NewPatch3, NewPatch4} };

                _ ->
                    { old, Field }
            end
    end;

% empty nodes and wrong leaves
find(Field, _Boundaries, _Pid) ->
    Field.

%% debug dump

indent(0) -> ok;

indent(N) ->
    io:format(" ", []),
    indent(N-1).

dump(GrassField) ->
    io:format("~n"),
    dump(GrassField, 0).

dump({empty, {X0, Y0}, {X1, Y1}}, Level) ->
    indent(Level),
    io:format("empty (~p, ~p) - (~p, ~p)~n", [ X0, Y0, X1, Y1 ]);

dump({leaf, {Xl, Yl}, {X0, Y0}, {X1, Y1}}, Level) ->
    indent(Level),
    io:format("leaf @ (~p, ~p) for (~p, ~p) - (~p, ~p)~n", [ Xl, Yl, X0, Y0, X1, Y1 ]);

dump({patch, {Xc, Yc}, Patch1, Patch2, Patch3, Patch4}, Level) ->
    indent(Level),
    io:format("patches with center @ (~p, ~p)~n", [ Xc, Yc ]),
    Level1 = Level + 1,
    dump(Patch1, Level1),
    dump(Patch2, Level1),
    dump(Patch3, Level1),
    dump(Patch4, Level1).

% vim:ts=4:sw=4:et

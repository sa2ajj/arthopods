-module(world_viewer).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/1, make_bug/1, move_bug/2]).

-export([init/1]).

-define(CELL_SIZE, 2).

% each dot (grass leaf) is represented with a 2x2 square
% each bug is represented by a 6x6 square

start(Size) ->
    register(world_viewer, spawn_link(?MODULE, init, [Size])).

make_bug(Coords) ->
    world_viewer ! { make_bug, self(), Coords },
    receive
        { made_bug, Bug } -> Bug
    end.

move_bug(Bug, Location) ->
    world_viewer ! { move_bug, self(), Bug, Location },
    receive
        { moved_bug } -> ok
    end.

grow_leaf(Location) ->
    world_viewer ! { grow_grass, self(), Location),
    receive
        { planted_grass, Leaf } -> Leaf
    end.

% stuff for that separate process

real_coords(Location) ->
    real_coords(Location, 1).

real_coords({X, Y}, Offset) ->
    {(X+Offset)*?CELL_SIZE, (Y+Offset)*?CELL_SIZE}.

bug_rect(Location) ->
    { RealX, RealY } = real_coords(Location, 0),
    [ {RealX, RealY}, {RealX+3*?CELL_SIZE-1, RealY+3*?CELL_SIZE-1} ].

grass_rect(Location) ->
    { RealX, RealY } = RealLocation = real_coords(Location),
    [ RealLocation, {RealX+?CELL_SIZE-1, RealY+?CELL_SIZE-1} ].

init(Size) ->
    Gs = gs:start(),
    {RealWidth, RealHeight} = real_coords(Size, 2),
    Window = gs:create(window, Gs, [ {width, RealWidth}, {height, RealHeight}, {title, "Arthopods World"}, {map, true} ]),
    Canvas = gs:create(canvas, Window, [ {x, 0}, {y, 0}, {width, RealWidth}, {height, RealHeight} ]),
    loop(Canvas).

loop(Canvas) ->
    receive
        { make_bug, Pid, Location } ->
            Maroon = {16#b0, 16#30, 16#60},
            Pid ! { made_bug, gs:create(rectangle, Canvas, [
                {coords, bug_rect(Location)},
                {bw, 1},
                {fg, Maroon},
                {fill, Maroon}
            ]) };

        { move_bug, Pid, Bug, Location } ->
            gs:config(Bug, [{coords, bug_rect(Location)}]),
            Pid ! { moved_bug };

        { grow_grass, Pid, Location } ->
            DarkGreen = {0, 16#64, 0},
            Pid ! { planted_grass, gs:create(rectangle, Canvas, [
                {coords, grass_rect(Location)},
                {bw, 1},
                {fg, DarkGreen},
                {fill, DarkGreen}
            ]) }
    end,
    loop(Canvas).

% vim:ts=4:sw=4:et

-module(food_generator).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/3, food_loop0/3]).

start(World, Ticks, Coverage) ->
    spawn_link(?MODULE, food_loop0, [World, Ticks, Coverage]).

food_loop0(World, Ticks, Coverage) when (Coverage >= 0) and (Coverage < 1) ->
    io:format("Food is unlimited!~n"),
    World ! {size, self()},
    receive
        {size, {Width, Height}=Size} ->
            io:format("World size is ~p~n", [Size]),
            InitialPieces = trunc(Width*Height*Coverage),
            io:format("Pre-populating ~p pieces...", [InitialPieces]),
            plant_grass(Size, World, InitialPieces),
            io:format(" done. ~nNow generating a piece of food every ~pmsecs~n", [Ticks]),
            food_loop(World, Ticks, Size);

        Other ->
            io:format("Food generator got: ~p~nFood generator cannot continue.~n", [Other])
    end.

food_loop(World, Ticks, Size) ->
    % io:format("Food for world of size ~p~n", [Size]),
    receive after Ticks ->
        plant_grass(Size, World, 1),
        food_loop(World, Ticks, Size)
    end.

plant_grass(_Size, _World, 0) ->
    void;
plant_grass(Size, World, N) ->
    Location = get_food(Size),
    % io:format("Food @ ~p~n", [{food, Location}]),
    World ! {food, Location},
    plant_grass(Size, World, N - 1).

get_food({Width, Height}) ->
    {random:uniform(Width), random:uniform(Height)}.

% vim:ts=4:sw=4:et

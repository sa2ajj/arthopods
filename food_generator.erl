%

-module(food_generator).

-export([start/2, food_loop0/2]).

start(World, Ticks) ->
    spawn_link(?MODULE, food_loop0, [World, Ticks]).

food_loop0(World, Ticks) ->
    io:format("Food is unlimited!~n"),
    World ! { size, self() },
    receive
        { size, Size } ->
            io:format("World size is ~p~n", [Size]),
            food_loop(World, Ticks, Size);

        Other ->
            io:format("Food generator got: ~p~nFood generator cannot continue.~n", [ Other ])
    end.

food_loop(World, Ticks, Size) ->
    % io:format("Food for world of size ~p~n", [Size]),
    receive after Ticks ->
        Location = get_food(Size),
        % io:format("Food @ ~p~n", [ { food, Location } ]),
        World ! { food, Location },
        food_loop(World, Ticks, Size)
    end.

get_food({ Width, Height }) ->
    { random:uniform(Width), random:uniform(Height) }.

% vim:ts=4:sw=4:et

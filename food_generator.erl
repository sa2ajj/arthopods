%

-module(food_generator).

-export([start/2, food_loop0/2]).

start(World, Ticks) ->
    spawn_link(?MODULE, food_loop0, [World, Ticks]).

food_loop0(World, Ticks) ->
    io:format("Food is unlimited!~n"),
    World ! { size, self() },
    receive
        { size, Size } -> food_loop(World, Ticks, Size);

        Other -> io:format("Food generator got: ~p~nFood generator cannot continue.~n", [ Other ])
    end.

food_loop(World, Ticks, Size) ->
    receive after Ticks ->
        World ! { food, get_food(Size) },
        food_loop(World, Ticks, Size)
    end.

get_food({ Width, Height })
    when is_integer(Width) andalso is_integer(Height) ->
    { random:uniform(Width), random:uniform(Height) }.

% vim:ts=4:sw=4:et

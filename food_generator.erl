%

-module(food_generator).

-export([start/2, food_loop0/2]).

start(World, Ticks) ->
    spawn_link(?MODULE, food_loop0, [World, Ticks]).

food_loop0(World, Ticks) ->
    io:format("Food is unlimited!~n"),
    food_loop(World, Ticks).

food_loop(World, Ticks) ->
    receive
    after
        Ticks ->
            World ! { food },
            food_loop(World, Ticks)
    end.

% vim:ts=4:sw=4:et

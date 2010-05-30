-module(arthopod_simple).
-behaviour(arthopod).

% behaviour callbacks
-export([give_birth/2]).

% data structures
-record(simple_bug, {
    world,
    age,
    energy,
    direction,
    genes
}).

% behaviour callback implementation
give_birth(World, Energy) ->
    life(#simple_bug{world=World, age=0, energy=Energy}).

life(Bug) ->
    receive
    after 0 ->
        ok
    end,
    life(Bug).

% vim:ts=4:sw=4:et

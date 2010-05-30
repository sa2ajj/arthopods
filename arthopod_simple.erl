-module(arthopod_simple).
-behaviour(arthopod).

% behaviour callbacks
-export([give_birth/2]).

-export([life/1]).

% data structures
-record(simple_bug, {
    name,
    age,
    energy,
    direction,
    genes
}).

% behaviour callback implementation
give_birth(Name, Energy) ->
    spawn_link(?MODULE, life, [ #simple_bug{name=Name, age=0, energy=Energy} ]).

life(Bug) ->
    receive
    after 0 ->
        ok
    end,
    life(Bug).

% vim:ts=4:sw=4:et

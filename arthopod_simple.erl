-module(arthopod_simple).
-behaviour(arthopod).

-include("arthopod.hrl").

% behaviour callbacks
-export([give_birth/2]).

% behaviour callback implementation
give_birth(World, Energy) ->
    life(#simple_bug{
            world=World,
            age=0,          % new born :)
            energy=Energy,
            direction=arthopod:random_dir(),
            genes=make_genes(arthopod:directions(), 10)
    }).

% the actual worker
life(#simple_bug{energy=Energy}) when Energy == 0 ->
    ok;

life(#simple_bug{age=Age, energy=Energy, direction=Direction, genes=Genes} = Bug) ->
    io:format("Bug: ~p~n", [Bug]),
    timer:sleep(100),
%    receive
%    after 0 ->
%        ok
%    end,
    NewDirection = arthopod:turn(Direction, select:quadratic(Genes)),
    life(Bug#simple_bug{age=Age+1, energy=Energy-1, direction=NewDirection}).

% helper functions
make_genes(GeneTags, MaxValue) ->
    make_genes(GeneTags, MaxValue, []).

make_genes([], _, Result) ->
    Result;

make_genes([ GeneTag | Tail ], MaxValue, Result) ->
    make_genes(Tail, MaxValue, [{GeneTag, random:uniform(MaxValue)} | Result]).

% vim:ts=4:sw=4:et

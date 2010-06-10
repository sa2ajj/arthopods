-module(arthopod_simple).
-behaviour(arthopod).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-include("arthopod.hrl").

% behaviour callbacks
-export([give_birth/3]).

% behaviour callback implementation
give_birth(Body, Direction, Genes) ->
    process_flag(trap_exit, true),
    life(#simple_bug{
            body=Body,
            direction=Direction,
            genes=Genes
    }).

% the actual worker
life(#simple_bug{direction=Direction, genes=Genes} = Bug) ->
    io:format("Bug: ~p~n", [Bug]),
    timer:sleep(100),
%    receive
%    after 0 ->
%        ok
%    end,
    NewDirection = arthopod:turn(Direction, select:quadratic(Genes)),
    life(Bug#simple_bug{direction=NewDirection}).

% vim:ts=4:sw=4:et

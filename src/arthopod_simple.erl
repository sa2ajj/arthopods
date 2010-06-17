-module(arthopod_simple).
-behaviour(arthopod).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% behaviour callbacks
-export([give_birth/2]).

% behaviour callback implementation
give_birth(Body, Genes) ->
    process_flag(trap_exit, true),
    life(Body, Genes).

% the actual worker
life(Body, Genes) ->
    % io:format("Bug: ~p~n", [[Body, Genes]]),
    arthopod:eat(Body),
    arthopod:turn(Body),
    arthopod:move(Body),
    arthopod:split(Body),
    life(Body, Genes).

% vim:ts=4:sw=4:et

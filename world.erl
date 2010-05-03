%

-module(world).

-export([start/0, world_loop0/1]).

start() ->
    spawn(?MODULE, world_loop0, [ self() ]).

world_loop0(Parent) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),
    link(Parent),
    world_loop(Parent).

world_loop(Parent) ->
    receive
        { 'EXIT', Parent, _Reason } -> exit(normal);
        Other -> io:format("World got: ~w~n", Other), world_loop(Parent)
    end.

% vim:ts=4:sw=4:et

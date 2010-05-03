%

-module(world).

-export([start/1, world_loop0/2]).

start(Size) ->
    spawn(?MODULE, world_loop0, [ self(), Size ]).

world_loop0(Parent, Size) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),          % follow exits of interesting processes
    link(Parent),
    Parent ! { world, self() },
    world_loop(Parent, Size).

world_loop(Parent, Size) ->
    io:format("World: waiting... "),

    receive
        { 'EXIT', Parent, _Reason } ->
            io:format("~nWorld: exiting...~n"),
            exit(normal);

        { food } ->
            io:format(" food!~n"),
            world_loop(Parent, Size);

        Other ->
            io:format("World got: ~p~n", [ Other ]),
            world_loop(Parent, Size)
    end.

% vim:ts=4:sw=4:et

%

-module(world).

-export([start/0, world_loop0/1]).

start() ->
    spawn(?MODULE, world_loop0, [ self() ]).

world_loop0(Parent) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),          % follow exits of interesting processes
    link(Parent),
    Parent ! { world, self() },
    world_loop(Parent).

world_loop(Parent) ->
    io:format("World: waiting... "),

    receive
        { 'EXIT', Parent, _Reason } ->
            io:format("~nWorld: exiting...~n"),
            exit(normal);

        { food } ->
            io:format(" food!~n"),
            world_loop(Parent);

        Other ->
            io:format("World got: ~p~n", [ Other ]),
            world_loop(Parent)
    end.

% vim:ts=4:sw=4:et

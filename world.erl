%

-module(world).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/1, world_loop0/2]).

start(Size) ->
    spawn(?MODULE, world_loop0, [ self(), Size ]).

world_loop0(Parent, Size) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),          % follow exits of interesting processes
    link(Parent),
    Field = spawn(grass_field, start, [ lists:max(tuple_to_list(Size)) ]),
    Parent ! { world, self() },
    io:format("We are as big as ~p~n", [Size]),
    Bugs = dict:new(),
    world_loop(Parent, Size, Field, Bugs).

world_loop(Parent, Size, Field, Bugs) ->
    io:format("World: waiting... "),

    receive
        { 'EXIT', Parent, _Reason } ->
            io:format(" exiting...~n"),
            exit(normal);

        { welcome, _ } ->
            io:format(" welcomed.~n");

        { food, Location } ->
            io:format(" food (~p)!~n", [Location]),
            Field ! { grow, self(), Location };

        { size, Pid } ->
            io:format(" size requested from ~p~n", [ Pid ]),
            Pid ! { size, Size };

        ack_grow ->
            Field ! { dump, self() };

        ack_dump ->
            ok;

        Other ->
            io:format("World got: ~p~n", [ Other ])
    end,

    world_loop(Parent, Size, Field, Bugs).

% vim:ts=4:sw=4:et

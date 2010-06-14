%

-module(main).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/0]).

-define(WORLD_SIZE, {700, 400}).

start() ->
    io:format("great stuff will be here :)~n"),
    Size = ?WORLD_SIZE,
    world_viewer:start(Size),
    arthopod_sup:start_link(),      % TODO: how to do it properly?
    case world:start(Size) of
        {ok, World} ->
            io:format("World has been created (~p)~n", [World]),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, 200),       % generate a piece of food every 200 msecs
            populate_world(3),
            loop();
        Other ->
            io:format("Failed to create the world: ~p~n", [Other])
    end.

populate_world(0) ->
    ok;

populate_world(Number) ->
    world:give_birth(arthopod, [simple]),
    populate_world(Number-1).

loop() -> loop().

% vim:ts=4:sw=4:et

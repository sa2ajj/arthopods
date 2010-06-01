%

-module(main).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/0]).

-define(WORLD_SIZE, {480, 260}).

start() ->
    io:format("great stuff will be here :)~n"),
    Size = ?WORLD_SIZE,
    world_viewer:start(Size),
    case world:start(Size) of
        {ok, World} ->
            io:format("World has been created (~p)~n", [World]),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, 2000),      % generate a piece of food every 2 secs
            populate_world(World),
            loop();
        Other ->
            io:format("Failed to create the world: ~p~n", [Other])
    end.

populate_world(_World) ->
    ok.

loop() -> loop().

% vim:ts=4:sw=4:et

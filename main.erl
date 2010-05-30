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
    world:start(Size),
    receive
        { world, World } ->
            io:format("World has been created!~n"),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, 2000),      % generate a piece of food every 2 secs
            populate_world(World),
            loop();

        Other -> io:format("Got: ~w~n", Other)
    end.

populate_world(_World) ->
    ok.

loop() -> loop().

% vim:ts=4:sw=4:et

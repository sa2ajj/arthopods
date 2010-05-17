%

-module(main).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/0]).

start() ->
    io:format("great stuff will be here :)~n"),
    world:start({480, 260}),
    receive
        { world, World } ->
            io:format("World has been created!~n"),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, 2000),      % generate a piece of food every 2 secs
            populate_worle(World),
            loop();

        Other -> io:format("Got: ~w~n", Other)
    end.

populate_world(World) ->
    ok.

loop() -> loop().

% vim:ts=4:sw=4:et

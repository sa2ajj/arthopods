%

-module(main).

-export([start/0]).

start() ->
    io:format("great stuff will be here :)~n"),
    world:start({1024, 768}),
    receive
        { world, World } ->
            io:format("World has been created!~n"),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, 2000),      % generate a piece of food every 2 secs
            loop();

        Other -> io:format("Got: ~w~n", Other)
    end.

loop() -> loop().

% vim:ts=4:sw=4:et

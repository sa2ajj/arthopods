%

-module(main).

-export([start/0]).

start() ->
    io:format("great stuff will be here :)~n"),
    world:start(),
    loop().

loop() -> loop().

% vim:ts=4:sw=4:et

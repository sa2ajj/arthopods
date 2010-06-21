%

-module(main).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/0]).

-define(WORLD_SIZE, {700, 400}).
-define(FOOD_FREQUENCY, 20).        % generate a piece of food every 20 msecs
-define(BUGS_TO_CREATE, 1).

start() ->
    io:format("great stuff will be here :)~n"),
    random:seed(erlang:now()),
    Size = ?WORLD_SIZE,
    world_viewer:start_link(Size),
    arthopod_sup:start_link(),      % TODO: how to do it properly?
    case world:start(Size) of
        {ok, World} ->
            io:format("World has been created (~p)~n", [World]),
            World ! { welcome, "Thank you for being there" },
            food_generator:start(World, ?FOOD_FREQUENCY),
            populate_world(?BUGS_TO_CREATE),
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

-module(main).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

-export([start/0]).

start() ->
    io:format("great stuff will be here :)~n"),
    random:seed(erlang:now()),
    Size = application:get_env(world_size),
    world_viewer:start_link(Size),
    arthopod_sup:start_link(),      % TODO: how to do it properly?
    case world:start(Size) of
        {ok, World} ->
            io:format("World has been created (~p)~n", [World]),
            World ! {welcome, "Thank you for being there"},
            food_generator:start(World, application:get_env(food_frequency),
                                 application:get_env(initial_coverage)),
            populate_world(application:get_env(bugs_to_create)),
            loop();
        Other ->
            io:format("Failed to create the world: ~p~n", [Other])
    end.

populate_world(0) ->
    ok;

populate_world(Number) ->
    world:give_birth(arthopod, [simple]),
    populate_world(Number - 1).

loop() -> loop().

% vim:ts=4:sw=4:et

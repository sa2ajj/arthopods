%

-module(world).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% inteface definition
-export([start/1, stop/0, cast/1, call/1]).

% gen_server behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% interface implementation
start(Size) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self(), Size], []).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

call(Request) ->
    gen_server:call(?MODULE, Request).

% callback implementation
init([Parent, Size]) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),          % follow exits of interesting processes
    grass_field:start(lists:max(tuple_to_list(Size))),
    Parent ! { world, self() },
    io:format("We are as big as ~p~n", [Size]),
    Bugs = dict:new(),      % Pid -> {GsObject, Location}
    Grass = dict:new(),     % Location -> GsObject
    {ok, {Parent, Size, Bugs, Grass}}.

terminate(Reason, State) ->
    io:format("terminate: ~p, ~p~n", [Reason, State]),
    ok.

handle_call(Request, From, State) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, State]),
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

handle_cast(Request, State) ->
    io:format("handle_cast: ~p, ~p~n", [Request, State]),
    {noreply, State}.


handle_info({welcome, _}, State) ->
    io:format(" welcomed.~n"),
    {noreply, State};

handle_info({food, Location}, {Parent, Size, Bugs, Grass}) ->
    io:format(" food (~p)!~n", [Location]),
    NewGrass = case dict:find(Location, Grass) of
        {ok, _} ->
            io:format("We already have one at ~p~n", [Location]),
            Grass;

        error ->
            grass_field:grow(Location),
            Leaf = world_viewer:grow_leaf(Location),
            dict:store(Location, Leaf, Grass)
    end,

    {noreply, {Parent, Size, Bugs, NewGrass}};

handle_info({size, Pid}, {_, Size, _, _} = State) ->
    io:format(" size requested from ~p~n", [ Pid ]),
    Pid ! {size, Size},
    {noreply, State};

handle_info({ack_grow, _Location}, State) ->
    grass_field:dump(),
    {noreply, State};

handle_info(ack_dump, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("handle_info: ~p, ~p~n", [Info, State]),
    {noreply, State}.

% not implemented callbacks for gen_server
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% vim:ts=4:sw=4:et

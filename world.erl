%

-module(world).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% inteface definition
-export([start/1, stop/0, cast/1, call/1]).
-export([give_birth/2, die/1, move/2]).

% gen_server behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% {{{ definitions
-record(world_state, {
    parent,
    size,
    bugs,
    grass
}).
%% }}}

% interface implementation
start(Size) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self(), Size], []).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

cast(Request) ->
    gen_server:cast(?MODULE, Request).

call(Request) ->
    gen_server:call(?MODULE, Request).

give_birth(Species, Parameters) ->
    gen_server:call(?MODULE, {give_birth, Species, Parameters}).

die(Body) ->
    gen_server:cast(?MODULE, {die, Body}).

move(Body, Delta) ->
    gen_server:call(?MODULE, {move, Body, Delta}).

% callback implementation
init([Parent, Size]) ->
    io:format("The world is about to revolve.~n"),
    process_flag(trap_exit, true),          % follow exits of interesting processes
    grass_field:start(lists:max(tuple_to_list(Size))),
    Parent ! { world, self() },
    io:format("We are as big as ~p~n", [Size]),
    Bugs = dict:new(),      % Pid -> {GsObject, Location}
    Grass = dict:new(),     % Location -> GsObject
    {ok, #world_state{parent=Parent, size=Size, bugs=Bugs, grass=Grass}}.

terminate(Reason, State) ->
    io:format("terminate: ~p, ~p~n", [Reason, State]),
    ok.

handle_call({move, Body, {DX, DY}}, _From, #world_state{size={Width, Height}, bugs=Bugs} = State) ->
    {Reply, NewBugs} = case dict:find(Body, Bugs) of
        {ok, {BodyObject, {X, Y}}} ->
            NewLocation = {(X+DX+Width) rem Width, (Y+DY+Height) rem Height},
            world_viewer:move_bug(BodyObject, NewLocation),
            {ok, dict:store(Body, {BodyObject, NewLocation}, Bugs)};

        error ->
            {error, Bugs}
    end,
    {reply, Reply, State#world_state{bugs=NewBugs}};

handle_call({give_birth, Species, Parameters}, _From, #world_state{size={Width, Height}, bugs=Bugs} = State) ->
    case apply(Species, give_birth, Parameters) of
        {ok, BugBody} ->
            Location = {random:uniform(Width)-1, random:uniform(Height)-1},
            BodyObject = world_viewer:make_bug(Location),
            {reply, ok, State#world_state{bugs=dict:store(BugBody, {BodyObject, Location}, Bugs)}};

        {error, _Error} ->
            {reply, ok, State}
    end;

handle_call(Request, From, State) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, State]),
    {noreply, State}.

handle_cast({die, Body}, #world_state{bugs=Bugs} = State) ->
    case dict:find(Body, Bugs) of
        {ok, {BodyObject, _Location}} ->
            world_viewer:kill_bug(BodyObject),
            {noreply, State#world_state{bugs=dict:erase(Body, Bugs)}};

        error ->
            {noreply, State}
    end;

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

handle_cast(Request, State) ->
    io:format("handle_cast: ~p, ~p~n", [Request, State]),
    {noreply, State}.


handle_info({welcome, _}, State) ->
    io:format(" welcomed.~n"),
    {noreply, State};

handle_info({food, Location}, #world_state{grass=Grass} = State) ->
    io:format(" food (~p)!~n", [Location]),
    case dict:find(Location, Grass) of
        {ok, _} ->
            io:format("We already have one at ~p~n", [Location]),
            {noreply, State};

        error ->
            grass_field:grow(Location),
            Leaf = world_viewer:grow_leaf(Location),
            {noreply, State#world_state{grass=dict:store(Location, Leaf, Grass)}}
    end;

handle_info({size, Pid}, #world_state{size=Size} = State) ->
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

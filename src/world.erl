%

-module(world).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% inteface definition
-export([start/1, stop/0, cast/1, call/1]).
-export([give_birth/2, die/1, move/2, eat/1, split/2]).

-export([collect_leaves/2]).

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

eat(Body) ->
    gen_server:call(?MODULE, {eat, Body}).

split(Body, BugSpecs) ->
    gen_server:call(?MODULE, {split, Body, BugSpecs}).

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
    case give_birth(Species, Parameters, {random:uniform(Width)-1, random:uniform(Height)-1}, Bugs) of
        {ok, NewBugs} ->
            {reply, ok, State#world_state{bugs=NewBugs}};

        {error, _Error} ->
            {reply, ok, State}
    end;

handle_call({eat, Body}, _From, #world_state{bugs=Bugs, grass=Grass} = State) ->
    case dict:find(Body, Bugs) of
        {ok, {_, {X, Y}}} ->
            % io:format("looking for food around ~p~n", [{X, Y}]),
            case find_leaves({{X-1, Y-1}, {X+1, Y+1}}) of
                [] ->
                    % io:format("  found no leaves~n"),
                    {reply, 0, State};

                Leaves ->
                    % io:format("  found leaves: ~p~n", [Leaves]),
                    NewGrass = cut_leaves(Leaves, Grass),
                    {reply, length(Leaves), State#world_state{grass=NewGrass}}
            end;

        error ->
            {reply, 0, State}
    end;

handle_call({split, Body, BugSpecs}, _From, #world_state{bugs=Bugs} = State) ->
    case die(Body, Bugs) of
        {ok, Location, Bugs0} ->
            NewBugs = lists:foldl(fun
                ({Species, Parameters}, BugsIn) ->
                    % we do not care about the error code as the new dictionary is always returned
                    {_, BugsOut} = give_birth(Species, Parameters, Location, BugsIn),
                    BugsOut
            end, Bugs0, BugSpecs),
            {reply, ok, State#world_state{bugs=NewBugs}};

        {error, _} ->
            {reply, not_found, State}
    end;

handle_call(Request, From, State) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, State]),
    {noreply, State}.

handle_cast({die, Body}, #world_state{bugs=Bugs} = State) ->
    case die(Body, Bugs) of
        {ok, _Location, NewBugs} ->
            {noreply, State#world_state{bugs=NewBugs}};

        {error, _} ->
            {noreply, State}
    end;

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

handle_cast(Request, State) ->
    io:format("handle_cast: ~p, ~p~n", [Request, State]),
    {noreply, State}.

handle_info({welcome, How}, State) ->
    io:format("Welcomed: ~p~n", [How]),
    {noreply, State};

handle_info({food, Location}, #world_state{grass=Grass} = State) ->
    % io:format(" food @ ~p~n", [Location]),
    case dict:find(Location, Grass) of
        {ok, _} ->
            % io:format("We already have one at ~p~n", [Location]),
            {noreply, State};

        error ->
            grass_field:grow(Location),
            Leaf = world_viewer:grow_leaf(Location),
            {noreply, State#world_state{grass=dict:store(Location, Leaf, Grass)}}
    end;

handle_info({size, Pid}, #world_state{size=Size} = State) ->
    % io:format(" size requested from ~p~n", [ Pid ]),
    Pid ! {size, Size},
    {noreply, State};

handle_info({ack_grow, _Location}, State) ->
    % grass_field:dump(),
    {noreply, State};

handle_info(ack_dump, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("handle_info: ~p, ~p~n", [Info, State]),
    {noreply, State}.

% not implemented callbacks for gen_server
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% {{{ HELPER FUNCTIONS

give_birth(Species, Parameters, Location, Bugs) ->
    case apply(Species, give_birth, Parameters) of
        {ok, BugBody} ->
            BodyObject = world_viewer:make_bug(Location),
            {ok, dict:store(BugBody, {BodyObject, Location}, Bugs)};

        {error, _Error} ->
            {error, Bugs}
    end.

die(Body, Bugs) ->
    case dict:find(Body, Bugs) of
        {ok, {BodyObject, Location}} ->
            world_viewer:kill_bug(BodyObject),
            {ok, Location, dict:erase(Body, Bugs)};

        error ->
            {error, Bugs}
    end.

find_leaves(Boundaries) ->
    spawn(?MODULE, collect_leaves, [self(), Boundaries]),
    receive
        {leaves, Leaves} ->
            Leaves
    end.

collect_leaves(Parent, Boundaries) ->
    grass_field:find(Boundaries),
    do_collect_leaves(Parent, []).

do_collect_leaves(Parent, Result) ->
    receive
        {ack_find} ->
            Parent ! {leaves, Result},
            exit(normal);
        {leaf, Leaf} ->
            do_collect_leaves(Parent, [Leaf | Result])
    end.

cut_leaves([], Grass) ->
    Grass;

cut_leaves([Leaf | Rest], Grass) ->
    case dict:find(Leaf, Grass) of
        {ok, LeafObject} ->
            world_viewer:cut_leaf(LeafObject),
            cut_leaves(Rest, dict:erase(Leaf, Grass));

        error ->        % this should never happen though...
            cut_leaves(Rest, Grass)
    end.

%% }}}

% vim:ts=4:sw=4:et

%
-module(arthopod).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% {{{ EXPORTS
% behaviour info for arthopod
-export([behaviour_info/1]).

% API export
-export([give_birth/1, give_birth/2, give_birth/3, spawn_one/3]).
-export([move/1, turn/1, eat/1, split/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% }}}

%% {{{ DEFINITIONS
% data structure
-record(arthopod_body, {
    subspecies, energy, brain, genes, direction
}).

% local defines
-define(PREFIX, "arthopod_").
-define(DIRECTIONS, [forward, right, strong_right, backward, strong_left, left]).
-define(DELTAS, [
    {forward, {0, 2}},
    {right, {2, 1}},
    {strong_right, {2, -1}},
    {backward, {0, -2}},
    {strong_left, {-2, -1}},
    {left, {-2, 1}}
]).
-define(MAX_GENE_VALUE, 10).
-define(ARTHOPOD_DEFAULT_ENERGY, 200).
-define(TURN_COST, 1).
-define(TURN_TIME, 20).
-define(MOVE_COST, 2).
-define(MOVE_TIME, 100).
-define(FOOD_ENERGY, 10).
-define(SPLIT_ENERGY, 700).
%% }}}

%% {{{ behaviour description
behaviour_info(callbacks) ->
    [{give_birth, 2}];

behaviour_info(_Other) ->
    undefined.
%% }}}

subspecies_module(Kind) ->
    list_to_atom(?PREFIX++atom_to_list(Kind)).

give_birth(Kind) ->
    give_birth(Kind, ?ARTHOPOD_DEFAULT_ENERGY).

give_birth(Kind, Energy) ->
    give_birth(Kind, Energy, make_genes()).

give_birth(Kind, Energy, Genes) ->
    case supervisor:start_child(arthopod_sup, [Kind, Energy, Genes]) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};

        {error, Error} ->
            {error, Error};

        Other ->
            {error, Other}
    end.

spawn_one(Kind, Energy, Genes) ->
    gen_server:start_link(?MODULE, [Kind, Energy, Genes], []).

%% {{{ CALLBACK IMPLEMENTATION

% constructor
init([Kind, Energy, Genes] = _Args) ->
    % io:format("init: ~p~n", [Args]),
    Module = subspecies_module(Kind),
    {ok, #arthopod_body{
        subspecies=Kind,
        energy=Energy,
        brain=spawn_link(Module, give_birth, [self(), Genes]),
        genes=Genes,
        direction=random_dir()
    }}.

% destructor
terminate(_Reason, _Body) ->
    % io:format("terminate: ~p, ~p~n", [Reason, Body]),
    world:die(self()).

handle_call(turn, _From, #arthopod_body{energy=Energy} = Body) when Energy < ?TURN_COST ->
    {stop, normal, no_energy, Body};

handle_call(turn, _From, #arthopod_body{direction=Direction, energy=Energy, genes=Genes} = Body) ->
    NewDirection = turn(Direction, select:quadratic(Genes)),
    timer:sleep(?TURN_TIME),
    {reply, ok, Body#arthopod_body{direction=NewDirection, energy=Energy-?TURN_COST}};

handle_call(move, _From, #arthopod_body{energy=Energy} = Body) when Energy < ?MOVE_COST ->
    {stop, normal, no_energy, Body};

handle_call(move, _From, #arthopod_body{energy=Energy, direction=Direction} = Body) ->
    case lists:keyfind(Direction, 1, ?DELTAS) of
        false ->
            {stop, normal, unknown_direction, Body};

        {Direction, Delta} ->
            case world:move(self(), Delta) of
                ok ->
                    timer:sleep(?MOVE_TIME),
                    {reply, ok, Body#arthopod_body{energy=Energy-?MOVE_COST}};

                error ->
                    {stop, normal, not_known_to_world, Body}    % world does not know us? commit suicide!
            end
    end;

handle_call(eat, _, #arthopod_body{energy=Energy} = Body) ->
    case world:eat(self()) of
        0 ->
            {reply, ok, Body};

        LeafNo ->
            io:format("Got ~p pieces of food.~n", [LeafNo]),
            {reply, ok, Body#arthopod_body{energy=Energy+?FOOD_ENERGY*LeafNo}}
    end;

handle_call(split, _, #arthopod_body{subspecies=Kind, energy=Energy, genes=Genes} = Body) ->
    if
        Energy >= ?SPLIT_ENERGY ->
            {Genes1, Genes2} = mutate_genes(Genes),
            Energy1 = Energy div 2,
            Energy2 = Energy - Energy1,
            world:split(self(), [{arthopod, [Kind, Energy1, Genes1]}, {arthopod, [Kind, Energy2, Genes2]}]),
            {stop, normal, spawn_children, Body};

        true ->
            {reply, ok, Body}
    end;

handle_call(Request, From, Body) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, Body]),
    {reply, ok, Body}.

handle_cast({stop, Reason}, Body) ->
    {stop, Reason, Body};

handle_cast(Request, Body) ->
    io:format("handle_cast: ~p, ~p~n", [Request, Body]),
    {noreply, Body}.

% not implemented callbacks for gen_server
handle_info(Info, Body) ->
    io:format("handle_info: ~p, ~p~n", [Info, Body]),
    {noreply, Body}.

code_change(_OldVsn, Body, _Extra) ->
    {ok, Body}.

%% }}}

%% HELPER FUNCTIONS
random_dir() -> select:uniform(?DIRECTIONS).

turn(Body) when is_pid(Body) ->
    gen_server:call(Body, turn).

move(Body) when is_pid(Body) ->
    gen_server:call(Body, move).

eat(Body) when is_pid(Body) ->
    gen_server:call(Body, eat).

split(Body) when is_pid(Body) ->
    gen_server:call(Body, split).

turn(Direction, Turn) ->
    turn(Direction, Turn, ?DIRECTIONS).

turn(Direction, Turn, Directions) ->
    DirectionValue = utils:index(Direction, Directions)-1,
    TurnValue = utils:index(Turn, Directions)-1,
    lists:nth(((DirectionValue+TurnValue) rem length(Directions))+1, Directions).

make_genes() ->
    [ {Gene, random:uniform(?MAX_GENE_VALUE)} || Gene <- ?DIRECTIONS ].

mutate_genes(Genes) ->
    {Gene, Value} = lists:nth(random:uniform(length(Genes)), Genes),

    IncGene = fun
        ({X, Y}) when X==Gene -> {Gene, Y+1};
        ({X, Y}) -> {X, Y}
    end,

    DecGene = fun
        ({X, Y}) when X==Gene -> {Gene, Y-1};
        ({X, Y}) -> {X, Y}
    end,

    IncOthers = fun
        ({X, Y}) when X==Gene -> {Gene, Y};
        ({X, Y}) -> {X, lists:min([Y+1, ?MAX_GENE_VALUE])}
    end,

    DecOthers = fun
        ({X, Y}) when X==Gene -> {Gene, Y};
        ({X, Y}) -> {X, lists:max([Y-1, ?MAX_GENE_VALUE])}
    end,

    case Value of
        0 ->
            {lists:map(IncGene, Genes), lists:map(IncOthers, Genes)};

        ?MAX_GENE_VALUE ->
            {lists:map(DecOthers, Genes), lists:map(DecGene, Genes)};

        _ ->
            {lists:map(IncGene, Genes), lists:map(DecGene, Genes)}
    end.

% vim:ts=4:sw=4:et

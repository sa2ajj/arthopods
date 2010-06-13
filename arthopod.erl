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
-export([move/1, turn/1]).

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
    {forward, {0, 1}},
    {right, {1, 1}},
    {strong_right, {1, -1}},
    {backward, {0, -1}},
    {strong_left, {-1, -1}},
    {left, {-1, 1}}
]).
-define(MAX_GENE_VALUE, 10).
-define(ARTHOPOD_DEFAULT_ENERGY, 200).
-define(TURN_COST, 1).
-define(TURN_TIME, 20).
-define(MOVE_COST, 2).
-define(MOVE_TIME, 100).
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
init([Kind, Energy, Genes] = Args) ->
    io:format("init: ~p~n", [Args]),
    Module = subspecies_module(Kind),
    {ok, #arthopod_body{
        subspecies=Kind,
        energy=Energy,
        brain=spawn_link(Module, give_birth, [self(), Genes]),
        genes=Genes,
        direction=random_dir()
    }}.

% destructor
terminate(Reason, Body) ->
    io:format("terminate: ~p, ~p~n", [Reason, Body]),
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

turn(Direction, Turn) ->
    turn(Direction, Turn, ?DIRECTIONS).

turn(Direction, Turn, Directions) ->
    DirectionValue = utils:index(Direction, Directions)-1,
    TurnValue = utils:index(Turn, Directions)-1,
    lists:nth(((DirectionValue+TurnValue) rem length(Directions))+1, Directions).

make_genes() ->
    [ {Gene, random:uniform(?MAX_GENE_VALUE)} || Gene <- ?DIRECTIONS ].

% vim:ts=4:sw=4:et

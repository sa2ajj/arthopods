%

-module(arthopod).
-behaviour(supervisor).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% interface description
-export([start_link/0]).
-export([behaviour_info/1]).

-export([give_birth/3, spawn_one/3]).

% helper functions for our beloved arthopods
-export([move/2, turn/2, random_dir/0]).

% supervisor callbacks
-export([init/1]).

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

% interface implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, none).

behaviour_info(callbacks) ->
    [{give_birth, 2}];

behaviour_info(_Other) ->
    undefined.

give_birth(Kind, World, Energy) ->
    case supervisor:start_child(?MODULE, [list_to_atom(?PREFIX++atom_to_list(Kind)), World, Energy]) of
        Pid when is_pid(Pid) ->
            {ok, Pid};

        {error, Error} ->
            {error, Error};

        Other ->
            {error, Other}
    end.

% supervisor callback implementation
init(none) ->
    { ok, {
        {simple_one_for_one, 0, 1},
        [
            {
                spawn_one,  % id -- not used anywhere
                {arthopod, spawn_one, []},
                temporary, brutal_kill, worker, dynamic
            }
        ]
    }}.

% helper functions

spawn_one(Module, World, Energy) ->
    spawn_link(Module, give_birth, [ World, Energy ]).

random_dir() -> select:uniform(?DIRECTIONS).

move(Location, Direction) ->
    move(Location, Direction, ?DELTAS).

move({X, Y}, Direction, Deltas) ->
    {_, {DX, DY}} = lists:keyfind(Direction, 1, Deltas),
    {X+DX, Y+DY}.

turn(Direction, Turn) ->
    turn(Direction, Turn, ?DIRECTIONS).

turn(Direction, Turn, Directions) ->
    DirectionValue = utils:index(Direction, Directions)-1,
    TurnValue = utils:index(Turn, Directions)-1,
    lists:nth(((DirectionValue+TurnValue) rem length(Directions))+1, Directions).

% vim:ts=4:sw=4:et

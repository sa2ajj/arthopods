%
-module(arthopod).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% {{{ EXPORTS
% behaviour info for arthopod
-export([behaviour_info/1]).

% API export
-export([give_birth/3, spawn_one/3]).
-export([make_genes/0, move/2, turn/2, random_dir/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% }}}

%% {{{ DEFINITIONS
-include("arthopod.hrl").

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
%% }}}

%% {{{ behaviour description
behaviour_info(callbacks) ->
    [{give_birth, 3}];

behaviour_info(_Other) ->
    undefined.
%% }}}

give_birth(Kind, World, Energy) ->
    Result = supervisor:start_child(arthopod_sup, [list_to_atom(?PREFIX++atom_to_list(Kind)), World, Energy]),
    io:format("give_birth: ~p (~p)~n", [Result, is_pid(Result)]),
    case Result of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};

        {error, Error} ->
            {error, Error};

        Other ->
            {error, Other}
    end.

%% {{{ CALLBACK IMPLEMENTATION

% constructor
init([Module, World, Energy] = Args) ->
    io:format("init: ~p~n", [Args]),
    Genes = make_genes(),
    Brain = spawn_link(Module, give_birth, [self(), arthopod:random_dir(), Genes]),
    io:format(" brain @ ~p~n", [Brain]),
    {ok, {Module, World, Energy, Brain, Genes}}.

% destructor
terminate(Reason, State) ->
    io:format("terminate: ~p, ~p~n", [Reason, State]),
    ok.

handle_call(Request, From, State) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, State]),
    {reply, ok, State}.

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

handle_cast(Request, State) ->
    io:format("handle_cast: ~p, ~p~n", [Request, State]),
    {noreply, State}.

% not implemented callbacks for gen_server
handle_info(Info, State) ->
    io:format("handle_info: ~p, ~p~n", [Info, State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% }}}

%% HELPER FUNCTIONS
spawn_one(Module, World, Energy) ->
    gen_server:start_link(?MODULE, [Module, World, Energy], []).

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

make_genes() ->
    make_genes(?DIRECTIONS, ?MAX_GENE_VALUE).

make_genes(GeneTags, MaxValue) ->
    make_genes(GeneTags, MaxValue, []).

make_genes([], _, Result) ->
    Result;

make_genes([ GeneTag | Tail ], MaxValue, Result) ->
    make_genes(Tail, MaxValue, [{GeneTag, random:uniform(MaxValue)} | Result]).

% vim:ts=4:sw=4:et

%
-module(arthopod).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% {{{ EXPORTS
% behaviour info for arthopod
-export([behaviour_info/1]).

% API export
-export([give_birth/2, spawn_one/2]).
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
%% }}}

%% {{{ behaviour description
behaviour_info(callbacks) ->
    [{give_birth, 2}];

behaviour_info(_Other) ->
    undefined.
%% }}}

give_birth(Kind, Energy) ->
    Result = supervisor:start_child(arthopod_sup, [list_to_atom(?PREFIX++atom_to_list(Kind)), Energy]),
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
init([Module, Energy] = Args) ->
    io:format("init: ~p~n", [Args]),
    Genes = make_genes(),
    {ok, #arthopod_body{
        subspecies=Module,
        energy=Energy,
        brain=spawn_link(Module, give_birth, [self(), Genes]),
        genes=Genes,
        direction=random_dir()
    }}.

% destructor
terminate(Reason, State) ->
    io:format("terminate: ~p, ~p~n", [Reason, State]),
    ok.

handle_call(turn, _From, #arthopod_body{direction=Direction, genes=Genes} = Body) ->
    NewDirection = turn(Direction, select:quadratic(Genes)),
    {reply, ok, Body#arthopod_body{direction=NewDirection}};

handle_call(move, _From, #arthopod_body{direction=Direction} = Body) ->
    case world:move(self(), lists:keyfind(Direction, 1, ?DELTAS)) of
        ok ->
            ok;

        error ->
            exit(normal)    % world does not know us? commit suicide!
    end,
    {reply, ok, Body};

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
spawn_one(Module, Energy) ->
    gen_server:start_link(?MODULE, [Module, Energy], []).

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
    make_genes(?DIRECTIONS, ?MAX_GENE_VALUE).

make_genes(GeneTags, MaxValue) ->
    make_genes(GeneTags, MaxValue, []).

make_genes([], _, Result) ->
    Result;

make_genes([ GeneTag | Tail ], MaxValue, Result) ->
    make_genes(Tail, MaxValue, [{GeneTag, random:uniform(MaxValue)} | Result]).

% vim:ts=4:sw=4:et
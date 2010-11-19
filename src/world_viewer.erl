-module(world_viewer).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% each dot (grass leaf) is represented with a 2x2 square
% each bug is represented by a 6x6 square
% TODO: find out a way to make it configurable
-define(CELL_SIZE, 2).

-record(viewer_state, {
    canvas,
    bug_colour,
    grass_colour
}).

%% server interface
-export([start_link/1, stop/0, make_bug/1, move_bug/2, kill_bug/1, grow_leaf/1, cut_leaf/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% {{{ interface implementation
start_link(Size) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Size, []).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

make_bug(Location) ->
    gen_server:call(?MODULE, {make_bug, Location}).

move_bug(Bug, Location) ->
    gen_server:cast(?MODULE, {move_bug, Bug, Location}).

kill_bug(Bug) ->
    gen_server:cast(?MODULE, {kill_bug, Bug}).

grow_leaf(Location) ->
    gen_server:call(?MODULE, {grow_leaf, Location}).

cut_leaf(Leaf) ->
    gen_server:cast(?MODULE, {cut_leaf, Leaf}).
%% }}}

%% gen_server callbacks implementation
init(Size) ->
    Gs = gs:start(),
    {RealWidth, RealHeight} = real_coords(Size, 2),
    Window = gs:create(window, Gs, [{width, RealWidth}, {height, RealHeight}, {title, "Arthopods World"}, {map, true}]),
    Canvas = gs:create(canvas, Window, [{x, 0}, {y, 0}, {width, RealWidth}, {height, RealHeight}]),
    {ok, #viewer_state{canvas=Canvas,
                       bug_colour=appllication:get_env(bug_colour),
                       grass_colour=appllication:get_env(grass_colour)}}.

terminate(_Reason, _State) ->
    % io:format("terminate: ~p, ~p~n", [_Reason, _State]),
    gs:stop().

handle_call({make_bug, Location}, _From, #viewer_state{canvas=Canvas, bug_colour=BugColour}=State) ->
    {reply, gs:create(rectangle, Canvas, [
        {coords, bug_rect(Location)},
        {bw, 1},
        {fg, BugColour},
        {fill, BugColour}
    ]), State};
handle_call({grow_leaf, Location}, _From, #viewer_state{canvas=Canvas, grass_colour=GrassColour}=State) ->
    {reply, gs:create(rectangle, Canvas, [
        {coords, grass_rect(Location)},
        {bw, 1},
        {fg, GrassColour},
        {fill, GrassColour}
    ]), State};
handle_call(Request, From, State) ->
    io:format("handle_call: ~p, ~p, ~p~n", [Request, From, State]),
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};
handle_cast({move_bug, Bug, Location}, State) ->
    gs:config(Bug, [{coords, bug_rect(Location)}]),
    {noreply, State};
handle_cast({kill_bug, Bug}, State) ->
    gs:destroy(Bug),
    {noreply, State};
handle_cast({cut_leaf, Leaf}, State) ->
    gs:destroy(Leaf),
    {noreply, State};
handle_cast(Request, State) ->
    io:format("handle_cast: ~p, ~p~n", [Request, State]),
    {noreply, State}.

% not implemented callbacks for gen_server
handle_info(Info, State) ->
    io:format("handle_info: ~p, ~p~n", [Info, State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% a few helper functions
real_coords(Location) ->
    real_coords(Location, 1).

real_coords({X, Y}, Offset) ->
    {(X+Offset)*?CELL_SIZE, (Y+Offset)*?CELL_SIZE}.

bug_rect(Location) ->
    {RealX, RealY} = real_coords(Location, 0),
    [{RealX, RealY}, {RealX+3*?CELL_SIZE-1, RealY+3*?CELL_SIZE-1}].

grass_rect(Location) ->
    {RealX, RealY} = RealLocation = real_coords(Location),
    [RealLocation, {RealX+?CELL_SIZE-1, RealY+?CELL_SIZE-1}].

% vim:ts=4:sw=4:et

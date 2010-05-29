% a skeleton for our servers
-module(my_server).
-behaviour(gen_server).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% server interface
-export([start/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% interface implementation

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, {stop, normal}).

% callback implementation

% constructor
init(Args) ->
    io:format("init: ~p~n", [Args]),
    {ok, {}}.

% destructor
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

% not implemented callbacks for gen_server
handle_info(Info, State) ->
    io:format("handle_info: ~p, ~p~n", [Info, State]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% vim:ts=4:sw=4:et

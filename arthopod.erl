%

-module(arthopod).
-behaviour(supervisor).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% interface description
-export([start_link/0]).
-export([behaviour_info/1]).

-export([give_birth/2]).

% supervisor callbacks
-export([init/1]).

-define(PREFIX, "arthopod_").

% interface implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, none).

behaviour_info(callbacks) ->
    [{give_birth, 2}];

behaviour_info(_Other) ->
    undefined.

%% {Id, StartFunc, Restart, Shutdown, Type, Modules}
%%     Id = term()
%%     StartFunc = {M, F, A}
%%         M = F = atom()
%%         A = [term()]
%%     Restart = permanent | transient | temporary
%%     Shutdown = brutal_kill | integer() >=0 | infinity
%%     Type = worker | supervisor
%%     Modules = [Module] | dynamic
%%         Module = atom()

% for our purposes we need to create something like
% { Id, {M, give_birth, Energy}, temporary, brutal_kill, worker, dynamic }

give_birth(Kind, Energy) ->
    Id = utils:gen_unique_id(?PREFIX),
    Module = list_to_atom(?PREFIX++atom_to_list(Kind)),
    Child = {
        Id,
        { Module, give_birth, [Id, Energy] },
        temporary,
        brutal_kill,
        worker,
        dynamic     % TODO: review this line
    },
    case supervisor:start_child(?MODULE, Child) of
        {ok, Pid} ->
            {Id, Pid};

        {error, Error} ->
            {error, Error};

        Other ->
            {error, Other}
    end.

% supervisor callback implementation
init(none) ->
    {
        ok,
        { {one_for_one, 5, 10}, [] }
    }.

% vim:ts=4:sw=4:et

%
-module(arthopods_sup).
-behaviour(supervisor).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% API
-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {
        {one_for_one, 5, 10},
        [
            ?CHILD(arthopod_sup, supervisor),
            ?CHILD(world_viewer, worker)
        ]
    }}.

% vim:ts=4:sw=4:et

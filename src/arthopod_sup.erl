-module(arthopod_sup).
-behaviour(supervisor).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

% interface description
-export([start_link/0]).

% supervisor callbacks
-export([init/1]).

% interface implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, none).

% supervisor callback implementation
init(none) ->
    {ok, {
        {simple_one_for_one, 0, 1},
        [
            {
                spawn_one,  % id -- not used anywhere
                {arthopod, spawn_one, []},
                temporary, brutal_kill, worker, dynamic
            }
        ]
    }}.

% vim:ts=4:sw=4:et

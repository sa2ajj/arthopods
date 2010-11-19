-module(arthopods).
-behaviour(application).
-behaviour(supervisor).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% {{{ API exports
-export([start_link/0]).
%% }}}

%% {{{ application callbacks exports
-export([start/2, stop/1]).
%% }}}

%% {{{ supervisor callbacks exports
-export([init/1]).
%% }}}

%% {{{ Definitions
% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% }}}

%% {{{ API implementation
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% }}}

%% {{{ application callbacks implementation
start(_StartType, _StartArgs) ->
    start_link().

stop(_State) ->
    ok.
%% }}}

%% {{{ supervisor callbacks implementation
init([]) ->
    {ok, {
        {one_for_one, 5, 10},
        [
            ?CHILD(arthopod_sup, supervisor),
            ?CHILD(world_viewer, worker)
        ]
    }}.
%% }}}

% vim:ts=4:sw=4:et

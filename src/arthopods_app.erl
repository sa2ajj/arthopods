%
-module(arthopods_app).
-behaviour(application).

-author("Alexey Vyskubov <alexey@mawhrin.net>").
-author("Mikhail Sobolev <mss@mawhrin.net>").

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    arthopods_sup:start_link().

stop(_State) ->
    ok.

% vim:ts=4:sw=4:et

-module(raceman).


-export([start/0, stop/0]).
%-export([reload_dispatch/0]).

start() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(asn1),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(raceman),

	reloader:start().

stop() ->
    ok.

%reload_dispatch() ->
%	{ok, Dispatch} = file:consult("./priv/dispatch.conf"),
%	application:set_env(webmachine, dispatch_list, Dispatch).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

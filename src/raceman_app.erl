-module(raceman_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    raceman_sup:start_link().

stop(_State) ->
    ok.

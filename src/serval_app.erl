-module(serval_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    serval_sup:start_link().

stop(_State) ->
    ok.


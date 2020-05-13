-module(serval_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

child(Module) ->
    {Module, {Module, start_link, []}, permanent, 10000, worker, [Module]}.

init([]) ->
    {ok, {{one_for_one, 10, 10}, [child(serval)]}}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


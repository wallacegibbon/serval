-module(serval_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

child_specs() ->
    [{serval, {serval, start_link, []}, permanent, 10000, worker, [serval]}].

init([]) ->
    {ok, {{one_for_one, 10, 10}, child_specs()}}.


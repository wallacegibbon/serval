-module(serval_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Static_fspath = application:get_env(serval, static_fspath, "/tmp"),
    Static_urlpath = application:get_env(serval, static_urlpath, "/static") ++ "/[...]",
    Api_urlpath = application:get_env(serval, api_urlpath, "") ++ "/:type/:module/:fn",
    Port = application:get_env(serval, port, 8080),
    serval_utils:debugfmt("listening on port ~w, static(url: ~s, path: ~s),"
			  "api(url: ~s)~n",
			  [Port, Static_urlpath, Static_fspath,
			   Api_urlpath]),
    Path_config = [{Static_urlpath, cowboy_static,
		    {dir, Static_fspath, [{mimetypes, cow_mimetypes, all},
					  {etag, false}]}},
		   {Api_urlpath, serval_api, []}],
    Dispatch = cowboy_router:compile([{'_', Path_config}]),
    cowboy:start_clear(serval_listener, [{port, Port}],
		       #{env => #{dispatch => Dispatch}}).

stop(_State) ->
    ok = cowboy:stop_listener(serval_listener),
    ok.


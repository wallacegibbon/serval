-module(serval).

-behaviour(gen_server).

-export([handle_call/3, handle_cast/2, handle_info/2,
	 init/1, terminate/2, code_change/3]).

-export([start_link/0, stop/0]).

-export([initialize/1, get_safemodules/0, set_safemodules/1]).

-define(SERVER, ?MODULE).


set_safemodules(Modules) ->
    gen_server:call(?MODULE, {set_safemodules, Modules}).

get_safemodules() ->
    gen_server:call(?MODULE, get_safemodules).

initialize(Configs) ->
    gen_server:call(?MODULE, {initialize, Configs}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).


mkroute(StaticPath, StaticUrl, ApiUrl) ->
    [{StaticUrl, cowboy_static,
      {dir, StaticPath, [{mimetypes, cow_mimetypes, all},
			 {etag, false}]}},
     {ApiUrl, serval_api, []}].


handle_call({initialize, Configs}, _From, #{started := false} = State) ->
    StaticPath = maps:get(static_fspath, Configs, "/tmp"),
    StaticUrl = maps:get(static_prefix, Configs, "/static/") ++ "[...]",
    ApiUrl = maps:get(api_prefix, Configs, "/") ++ ":type/:module/:fn",
    Port = maps:get(port, Configs, 8080),
    Dispatch = cowboy_router:compile([{'_', mkroute(StaticPath, StaticUrl,
						    ApiUrl)}]),
    {ok, _} = cowboy:start_clear(serval_listener, [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    {reply, ok, State#{started := true}};

handle_call({initialize, _}, _From, #{started := true} = State) ->
    {reply, already_started, State};

handle_call(get_safemodules, _From, #{safemodules := R} = State) ->
    {reply, R, State};

handle_call({set_safemodules, Modules}, _From, State) ->
    {reply, ok, State#{safemodules := Modules}};

handle_call(stop, _From, #{started := true} = State) ->
    ok = cowboy:stop_listener(serval_listener),
    {stop, normal, stopped, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


init([]) ->
    {ok, #{started => false, safemodules => []}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


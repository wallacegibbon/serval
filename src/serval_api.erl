-module(serval_api).

-export([init/2, terminate/3]).

%%----------------------------------------------------------------------
%% Http related things
%%----------------------------------------------------------------------

get_command(#{bindings := #{type := Type, module := Module, fn := Fn}}
	    = Req0) ->
    TypeAtom = extract_atom(Type),
    ModuleAtom = extract_atom(Module),
    FnAtom = extract_atom(Fn),
    check_safety(ModuleAtom),
    {ok, Argument, Req1} = cowboy_req:read_body(Req0),
    {Req1, {TypeAtom, ModuleAtom, FnAtom, Argument}}.

extract_atom(Binary) ->
    try
	binary_to_existing_atom(Binary, utf8)
    catch
	error:badarg ->
	    throw({serval, unknown_operation})
    end.

decode_apiargument(Rawbody) ->
    try
	check_params(jsonerl:decode_fromjson(fix_emptybody(Rawbody)))
    catch
	throw:{invalid_json, _V} = E ->
	    throw({serval, E})
    end.

check_safety(Module) ->
    Safemodules = application:get_env(serval, safe_modules, []),
    Safe = lists:any(fun(X) -> X =:= Module end, Safemodules),
    if not Safe ->
	   throw({serval, unsafe_operation});
       true ->
	   nil
    end.

check_params(Params) when not is_list(Params) ->
    throw({serval, non_list_params});
check_params(Params) ->
    Params.

fix_emptybody(<<>>) ->
    <<"[]">>;
fix_emptybody(A) ->
    A.

handle_cors(Req) ->
    R1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
				    <<"OPTIONS, GET, POST">>, Req),
    R2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
				    <<"*">>, R1),
    R3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
				    <<"content-type, authorization">>, R2),
    R4 = cowboy_req:set_resp_header(<<"access-control-max-age">>,
				    <<"864000">>, R3),
    R4.

api(Module, Function, Arguments) ->
    try
	apply(Module, Function, Arguments)
    catch
	error:undef ->
	    throw({serval, undefined_function})
    end.

handle_request(jsonerl, Module, Fn, Argument) ->
    R = api(Module, Fn, decode_apiargument(Argument)),
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {RespHeaders, jsonerl:encode_tojson(R)};
handle_request(raw, Module, Fn, Argument) ->
    R = api(Module, Fn, [Argument]),
    {#{}, ensure_binary(R)};
handle_request(_, _, _, _) ->
    {#{}, <<"invalid url">>}.

handle_common(#{method := Method} = Req0)
  when Method =/= <<"OPTION">> ->
    try
	{Req1, {Type, Module, Fn, Argument}} = get_command(Req0),
	{RespHeaders, Body} = handle_request(Type, Module, Fn, Argument),
	{Req1, RespHeaders, Body}
    catch
	throw:{serval, Info}:S->
	    serval_utils:debugfmt("<handle_common> ~p~nstacktrace: ~p~n",
				  [Info, S]),
	    {Req0, #{}, jsonerl:encode_tojson({error, Info})}
    end;
handle_common(Req0) ->
    {Req0, #{}, <<>>}.

%% stack traces for unknown error are printed
safe_handle(Req0) ->
    try
	handle_common(Req0)
    catch
	T:I:S ->
	    serval_utils:debugfmt("<safe_handle> ~p:~p~nstacktrace: ~p~n",
				  [T, I, S]),
	    {Req0, #{}, jsonerl:encode_tojson(server_error)}
    end.

init(Req0, State) ->
    T1 = os:system_time(millisecond),
    {Req1, Headers, Body} = safe_handle(Req0),
    Req2 = handle_cors(Req1),
    T2 = os:system_time(millisecond),
    Timecost = list_to_binary(io_lib:format("~w", [T2 - T1])),
    NewHeaders = Headers#{<<"api-cost">> => Timecost},
    Req3 = cowboy_req:reply(200, NewHeaders, Body, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%----------------------------------------------------------------------
%% term encoding and decoding
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% utilities
%%----------------------------------------------------------------------
ensure_binary(Any) when is_list(Any); is_binary(Any) ->
    list_to_binary(io_lib:format("~s", [Any]));
ensure_binary(Any) ->
    list_to_binary(io_lib:format("~p", [Any])).


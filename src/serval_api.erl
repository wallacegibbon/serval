-module(serval_api).

-export([init/2, terminate/3]).

-include_lib("eunit/include/eunit.hrl").


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
	    throw({serval, "unknown operation"})
    end.

decode_apiargument(Rawbody) ->
    try
	check_params(decode(fix_emptybody(Rawbody)))
    catch
	throw:{invalid_json, _V} ->
	    throw({serval, "argument is not valid"})
    end.

check_safety(Module) ->
    Safemodules = serval:get_safemodules(),
    Safe = lists:any(fun(X) -> X =:= Module end, Safemodules),
    if not Safe ->
	   throw({serval, "unsafe operation"});
       true ->
	   nil
    end.

check_params(Params) when not is_list(Params) ->
    throw({serval, "params have to be list"});
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
    io:format(">> calling ~s:~s with ~p~n", [Module, Function, Arguments]),
    try
	apply(Module, Function, Arguments)
    catch
	error:undef ->
	    throw({serval, "undefined"})
    end.

handle_request(jsonerl, Module, Fn, Argument) ->
    R = api(Module, Fn, decode_apiargument(Argument)),
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {RespHeaders, encode(R)};
handle_request(raw, Module, Fn, Argument) ->
    R = api(Module, Fn, [Argument]),
    {#{}, ensure_binary(R)}.

handle_common(Req0) ->
    try
	{Req1, {Type, Module, Fn, Argument}} = get_command(Req0),
	{RespHeaders, Body} = handle_request(Type, Module, Fn, Argument),
	Req2 = handle_cors(Req1),
	{Req2, RespHeaders, Body}
    catch
	throw:{serval, Info}->
	    {Req0, #{}, encode({error, ensure_binary(Info)})}
    end.

%% stack traces for unknown error are printed
safe_handle(Req0) ->
    try
	handle_common(Req0)
    catch
	T:Any:S ->
	    io:format("*~p: ~p, stacktrace:~n~p~n", [T, Any, S]),
	    {Req0, #{}, encode({fatal, ensure_binary(Any)})}
    end.

init(Req0, State) ->
    {Req1, Headers, Body} = safe_handle(Req0),
    Req2 = cowboy_req:reply(200, Headers, Body, Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%----------------------------------------------------------------------
%% term encoding and decoding
%%----------------------------------------------------------------------

encode(Anything) ->
    try
	jsone:encode(encode_1(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

encode_1(E) when is_atom(E) ->
    #{<<"t">> => <<"atom">>, <<"v">> => list_to_binary(atom_to_list(E))};
encode_1(E) when is_tuple(E) ->
    #{<<"t">> => <<"tuple">>, <<"v">> => encode_1(tuple_to_list(E))};
encode_1(E) when is_binary(E); is_number(E) ->
    E;
encode_1([E | Rest]) ->
    [encode_1(E) | encode_1(Rest)];
encode_1([]) ->
    [];
encode_1(V) ->
    throw({invalid_json, V}).


decode(Anything) ->
    try
	decode_1(jsone:decode(Anything))
    catch
	error:badarg ->
	    throw({invalid_json, Anything})
    end.

decode_1(#{<<"t">> := <<"atom">>, <<"v">> := V}) ->
    binary_to_existing_atom(V, utf8);
decode_1(#{<<"t">> := <<"tuple">>, <<"v">> := V}) ->
    list_to_tuple(decode_1(V));
decode_1(E) when is_binary(E); is_number(E) ->
    E;
decode_1([E | Rest]) ->
    [decode_1(E) | decode_1(Rest)];
decode_1([]) ->
    [];
decode_1(V) ->
    throw({invalid_json, V}).


-ifdef(TEST).

encode_decode_maps() ->
    [{1, <<"1">>},
     {a, <<"{\"t\":\"atom\",\"v\":\"a\"}">>},
     {<<"ab">>, <<"\"ab\"">>},
     {{1,2}, <<"{\"t\":\"tuple\",\"v\":[1,2]}">>},
     {[1,2], <<"[1,2]">>},
     {[1,[2,[3]]], <<"[1,[2,[3]]]">>},
     {[1,[2,[<<"blah">>]]], <<"[1,[2,[\"blah\"]]]">>}
    ].

encode_test_() ->
    lists:map(fun({V, T}) ->
		      ?_assert(encode(V) =:= T)
	      end, encode_decode_maps()).

decode_test_() ->
    lists:map(fun({V, T}) ->
		      ?_assert(decode(T) =:= V)
	      end, encode_decode_maps()).

-endif.

%%----------------------------------------------------------------------
%% utilities
%%----------------------------------------------------------------------
ensure_binary(Any) when is_list(Any); is_binary(Any) ->
    list_to_binary(io_lib:format("~s", [Any]));
ensure_binary(Any) ->
    list_to_binary(io_lib:format("~p", [Any])).


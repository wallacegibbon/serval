-module(serval_web).

-export([init/2, terminate/3]).

-include_lib("eunit/include/eunit.hrl").


%%----------------------------------------------------------------------
%% Http related things
%%----------------------------------------------------------------------

%% read the content from HTTP body and parse it (as JSON)
get_params(#{bindings := #{module := Module, fn := Fn}} = Req0) ->
    try
	ModuleAtom = binary_to_existing_atom(Module, utf8),
	FnAtom = binary_to_existing_atom(Fn, utf8),
	check_safety(ModuleAtom),
	{ok, Data, Req1} = cowboy_req:read_body(Req0),
	Params = check_params(decode(fix_emptybody(Data))),
	{Req1, {ModuleAtom, FnAtom, Params}}
    catch
	throw:{invalid_json, _V} ->
	    throw({serval, "argument encoding error"});
	error:badarg ->
	    throw({serval, "unknown fn"})
    end.

check_safety(Module) ->
    Safemodules = serval_ctl:get_safemodules(),
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

record_request(Req) ->
    #{bindings := #{module := Module, fn := Fn}, peer := Peer} = Req,
    ForwardFor = maps:get(<<"x-forwarded-for">>, Req, none),
    io:format("Peer: ~p; Proxy: ~s; For: ~s->~s~n",
	      [Peer, ForwardFor, Module, Fn]).

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

api(Module, Function, Params) ->
    try
	apply(Module, Function, Params)
    catch
	error:undef ->
	    throw({serval, "argument error"})
    end.

handle_request(Req0) ->
    record_request(Req0),
    {Req1, {Module, Fn, Params}} = get_params(Req0),
    Result = api(Module, Fn, Params),
    Req2 = handle_cors(Req1),
    {Req2, Result}.

safe_handle(Req0) ->
    try
	handle_request(Req0)
    of
	{Req1, Result} ->
	    {Req1, {ok, Result}}
    catch
	throw:{serval, Info}->
	    {Req0, {failed, ensure_list(Info)}};
	_:Any:S ->
	    io:format("stacktrace:~n~p~n", [S]),
	    {Req0, {failed, ensure_list(Any)}}
    end.

init(Req0, State) ->
    {Req1, R} = safe_handle(Req0),
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    Req2 = cowboy_req:reply(200, RespHeaders, encode(R), Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%----------------------------------------------------------------------
%% term encoding and decoding
%%----------------------------------------------------------------------

encode(Anything) ->
    jsone:encode(encode_1(Anything)).

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
    decode_1(jsone:decode(Anything)).

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
ensure_list(Any) when is_list(Any) ->
    list_to_binary(Any);
ensure_list(Any) ->
    Any.


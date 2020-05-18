-module(serval_utils).

-export([debugfmt/2]).

debugfmt(Fmtstring, Arguments) ->
    case application:get_env(serval, debugfmt, true) of
	true ->
	    io:format(Fmtstring, Arguments);
	_ ->
	    ok
    end.


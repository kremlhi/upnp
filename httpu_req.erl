-module(httpu_req).

-export([parse/1]).

parse(Msg) ->
	try parse2(Msg)
	catch error:Reason ->
		{error,Reason} end.

parse2(Msg) ->
	[H, Body] = re:split(Msg, "\\r\\n\\r\\n", [{parts,2}]),
	[Req | H2] = re:split(H, "\\r\\n"),
	{ok,Req2} = parse_method(Req),
	{ok,H3} = parse_headers(H2),
	{ok,[{request,Req2}, {headers,H3}, {body,Body}]}.

parse_method(Line) ->
	M = re:split(Line, "\\s"),
	ok = validate_method(M),
	{ok,M}.

parse_headers(L) ->
	Headers = [begin
		[K,V] = re:split(X, ":[\\s\\t]*", [{parts,2}]),
		K2 = bin_to_upper(K),
		ok = validate_header(K2),
		{K2,V} end || X <- L],
	ok = validate_headers(Headers),
	{ok,Headers}.
	
validate_method([<<"NOTIFY">>, <<"*">>, Vsn]) ->
	ok = validate_vsn(Vsn);
validate_method([<<"M-SEARCH">>, <<"*">>, Vsn]) ->
	ok = validate_vsn(Vsn);
validate_method(M) ->
	erlang:error(badmethod, M).

validate_vsn(<<"HTTP/1.1">>) ->
	ok;
validate_vsn(<<"HTTP/1.0">>) ->
	ok;
validate_vsn(Vsn) ->
	erlang:error(badvsn, Vsn).

validate_header(_) -> ok.

validate_headers(L) ->
%%	[_] = [ok || {K,_V} <- L, K == <<"NTS">>],
	ok.

bin_to_upper(B) ->
	<< <<(if X >= $a, X =< $z -> X - $a + $A;
		true -> X end):8>>  || <<X:8>> <= B >>.


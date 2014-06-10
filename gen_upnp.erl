-module(gen_upnp).
-version("0.1").
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([connect/0, show_state/0]).

-compile([export_all]).

-record(state, {
	sock,
	notif = []}).

-record(httpu, {
	req,
	resp,
	hdrs,
	body}).

start_link() -> start_link([]).

start_link(Args) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).


init(_Opts) ->
	{ok,#state{}}.

handle_call({remove,Key}, _From, #state{notif = Notif} = State) ->
	{reply,ok,State#state{notif = proplists:delete(Key, Notif)}};
handle_call(connect, _From, #state{sock = undefined} = State) ->
	Opts = [{broadcast,true},
		{multicast_if,{0,0,0,0}},
		{add_membership,{{239,255,255,250},{0,0,0,0}}},
		{reuseaddr,true}],
	{ok,S} = gen_udp:open(1900, Opts),
	{reply,ok,State#state{sock = S}};
handle_call(show_state, _From, State) ->
	{reply,{ok,State},State}.

%%handle_info(_,_) -> ok.
handle_info({udp,S,_,_,"quit\n"}, State) ->
	io:format("bye!~n"),
	gen_udp:close(S),
	{stop,recvbye,State};
handle_info({udp,S,IP,Port,Packet = "NOTIFY * HTTP/1.1\r\n" ++ _}, 
		#state{sock = S, notif = Notif1} = State) ->
	{ok,Httpu} = upnp_parse(Packet), % Will crash if crappy packet
	#httpu{hdrs = Hdrs} = Httpu,
	Notif2 = case proplists:get_value("NTS", Hdrs) =/= "ssdp:alive" orelse
			proplists:is_defined({IP,Port}, Notif1) of
		false ->
			io:format("new client: ~p~n", [{IP,Port}]),
			[{{IP,Port},Packet} | Notif1];
		true -> Notif1
	end,		
	{noreply,State#state{notif = Notif2}};
handle_info({udp,S,IP,Port,Packet = "M-SEARCH * HTTP/1.1\r\n" ++ _},
		#state{sock = S} = State) ->
	io:format("Pkt: ~p~n~s~n", [{udp,S,IP,Port}, Packet]),
	ok = gen_udp:send(S, IP, Port, "HTTP/1.1 200 OK\r\n"
"CACHE-CONTROL:max-age=1800\r\n"
"EXT:\r\n"
"LOCATION:http://10.0.0.138:80/IGD.xml\r\n"
"SERVER:SpeedTouch 510 4.0.0.9.0 UPnP/1.0 (DG233B00011961)\r\n"
"ST:urn:schemas-upnp-org:service:WANPPPConnection:1\r\n"
"USN:uuid:UPnP-SpeedTouch510::urn:schemas-upnp-org:service:WANPPPConnection:1\r\n\r\n"),
	{noreply,State};
handle_info({udp,S,IP,Port,Packet}, State) ->
	io:format("Pkt: ~p~n~s~n", [{udp,S,IP,Port}, Packet]),
	{noreply,State};
handle_info(Else, State) ->
	io:format("Unexpected message: ~p~n", [Else]),
	{noreply,State}.

code_change(_, State, _Extra) ->
	{ok,State}.

terminate(_Reason, _State) -> ok.

connect() ->
	gen_server:call(?MODULE, connect).

show_state() ->
	gen_server:call(?MODULE, show_state).

%% Ugly as hell :)
%% TODO:
%% * strip both
%% * req/resp
%% * uppercase key
upnp_parse(S) ->
	[H,Body] = re:split(S, "\\r\\n\\r\\n", [{return,list}]),
	[Req,H2] = re:split(H, "\\r\\n", [{return,list}, {parts,2}]),
	H3 = re:split(H2, "\\r\\n", [{return,list}]),
	HP = [list_to_tuple(re:split(X, ":[\\s\\t]*", [{return,list}, {parts,2}])) || X <- H3],
	{ok,#httpu{req = Req, hdrs = HP, body = Body}}.
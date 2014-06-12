-module(gen_upnp).
-version("0.1").
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([connect/0, show_state/0]).

-record(state, {
	sock,
	notif = []}).

start_link() -> start_link([]).

start_link(Args) ->
	gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).


init(_Opts) ->
	{ok,#state{}}.

handle_call({remove,Key}, _From, #state{notif = Notif} = State) ->
	{reply,ok,State#state{notif = proplists:delete(Key, Notif)}};
handle_call(connect, _From, #state{sock = undefined} = State) ->
	Opts = [% {broadcast,true},
		{multicast_loop,false},
		{multicast_ttl,4},
		{multicast_if,{0,0,0,0}},
		{add_membership,{{239,255,255,250},{0,0,0,0}}},
		{reuseaddr,true},
		binary],
	{ok,S} = gen_udp:open(1900, Opts),
	{reply,ok,State#state{sock = S}};
handle_call(show_state, _From, State) ->
	{reply,{ok,State},State};
handle_call({send,IP,Port,Packet}, _From, #state{sock = S} = State) ->
	Ret = gen_udp:send(S, IP, Port, Packet),
	{reply,Ret,State}.

%%handle_info(_,_) -> ok.
handle_info({udp,S,_,_,<<"quit", _/binary>>}, State) ->
	io:format("bye!~n"),
	gen_udp:close(S),
	{stop,recvbye,State};
handle_info({udp,S,IP,Port,Packet}, State) ->
	case httpu_req:parse(Packet) of
		{ok,Packet2} ->
			Headers = proplists:get_value(headers,  Packet2),
			NTS = proplists:get_value(<<"NTS">>, Headers),
			State2 = handle_httpu({udp,S,IP,Port,Packet2}, NTS, State);
		_Error ->
			io:format("Ignoring crappy packet ~p~n~s~n", [{udp,S,IP,Port}, Packet]),
			State2 = State		
	end,		
	{noreply,State2};
handle_info(Else, State) ->
	io:format("Unexpected message: ~p~n", [Else]),
	{noreply,State}.

code_change(_, State, _Extra) ->
	{ok,State}.

terminate(_Reason, _State) -> ok.


handle_httpu({udp,S,IP,Port,Packet = [{request,[<<"NOTIFY">> | _]} | _]}, <<"ssdp:alive">>, State) ->
	Key = {udp,S,IP,Port},
	#state{notif = Notif} = State,
	case proplists:is_defined(Key, Notif) of
		false ->
			io:format("new client: ~p~n~p~n", [Key, Packet]),
			State#state{notif = [{Key,Packet} | Notif]};
		true -> State
	end;
handle_httpu({udp,S,IP,Port,[{request,[<<"NOTIFY">> | _]} | _]}, <<"ssdp:byebye">>, State) ->
	Key = {udp,S,IP,Port},
	#state{notif = Notif} = State,
	Notif2 = proplists:delete(Key, Notif),
	State#state{notif = Notif2};
handle_httpu({udp,S,IP,Port,[{request,[<<"M-SEARCH">> | _]} | _]}, _, State) ->
	ok = gen_udp:send(S, IP, Port, <<"HTTP/1.1 200 OK\r
CACHE-CONTROL:max-age=1800\r
EXT:\r
LOCATION:http://10.0.0.138:80/IGD.xml\r
SERVER:SpeedTouch 510 4.0.0.9.0 UPnP/1.0 (DG233B00011961)\r
ST:urn:schemas-upnp-org:service:WANPPPConnection:1\r
USN:uuid:UPnP-SpeedTouch510::urn:schemas-upnp-org:service:WANPPPConnection:1\r\n\r\n">>),
	State.

connect() ->
	gen_server:call(?MODULE, connect).

show_state() ->
	gen_server:call(?MODULE, show_state).

send(IP, Port, Packet) ->
	gen_server:call(?MODULE, {send,IP,Port,Packet}).

send_search() ->
	send({239,255,255,250}, 1900, <<"M-SEARCH * HTTP/1.1\r
HOST: 239.255.255.250:1900\r
MAN: ssdp:discover\r
MX: 10\r
ST: ssdp:all\r\n\r\n">>).

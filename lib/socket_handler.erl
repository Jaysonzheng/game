%% Author: xiaoshengaya
%% Created: 2010-2-2
%% Description: 
-module(socket_handler).

-behaviour(gen_fsm).

-include("protocol.hrl").

-export([start_link/1, set_socket/3, decode_data/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export([
    'WAIT_FOR_SOCKET'/2,
	'WAIT_FOR_HANDSHAKE'/2,
    'WAIT_FOR_DATA'/2,
    'WAIT_FOR_ECHO'/2
]).

-record(state, {
                socket,    % client socket
                addr,      % client address
			   	server
               }).

-define(TIMEOUT, 300000).		%% 5  minutes
-define(WAIT_ECHO_TIME, 30000).	%% 30 seconds

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(Server) ->
    gen_fsm:start_link(?MODULE, [Server], []).

%%set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
set_socket(Pid, Socket, SocketId) when is_pid(Pid), is_port(Socket), is_integer(SocketId) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Server]) ->
    process_flag(trap_exit, true),
	{ok, 'WAIT_FOR_SOCKET', #state{server = Server}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
'WAIT_FOR_SOCKET'({socket_ready, Socket}, #state{server = Server} = State) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, 0}]),
	Server:handle_accept(Socket),
	{ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_HANDSHAKE', State#state{socket=Socket, addr=IP}, ?TIMEOUT};

'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg("State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n", [Other]),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_HANDSHAKE'({data, Data}, State) ->
	handshake({data, Data}, State).

'WAIT_FOR_DATA'({data, Data}, State) ->
	recv_data({data, Data}, State);

'WAIT_FOR_DATA'(timeout, #state{socket=Socket} = State) ->
%%	io:format("~p Client connection timeout - sending sync,waiting for echo.\n", [self()]),
%%  {stop, normal, State};
	SyncPacket = protocol:build_packet(?SERVER_CMD_SYNC),
	catch gen_tcp:send(Socket, SyncPacket),
    {next_state, 'WAIT_FOR_ECHO', State, ?WAIT_ECHO_TIME};

'WAIT_FOR_DATA'(_Data, State) ->
%%  io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.

'WAIT_FOR_ECHO'({data, Data}, State) ->
	recv_data({data, Data}, State);
			
'WAIT_FOR_ECHO'(timeout, State) ->
%%  error_logger:error_msg("~p Client connection timeout - closing.\n", [self()]),
    {stop, normal, State}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
	io:format("handle_info recv bin:~p~n", [Bin]),
	inet:setopts(Socket, [{active, once}, {packet, 0}]),
    ?MODULE:StateName({data, Bin}, StateData);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    io:format("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(_Info, _StateName, StateData) ->
    {stop, normal, StateData}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket, server=Server}) ->
    Server:handle_close(Socket),
	(catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

recv_data({data, Data}, #state{socket=Socket, server = Server} = State) ->
	io:format("recv data:~p~n",[Data]),
	Body = parse_data(binary_to_list(Data)),
	DeBody = decode_data(Body, <<>>),
	io:format("after parse:~p, DeBody = ~w~n", [Body, DeBody]),
	%%{Val, R} = protocol:read_short(DeBody),
	%%io:format("read short val = ~p~n", [Val]),
	Server:handle_read(Socket, DeBody),

	%% Enstr = base64:encode_to_string("hello"),
	%% gen_tcp:send(Socket, [0] ++ Enstr ++ [255]),
	{next_state, 'WAIT_FOR_DATA', State}.

handshake({data, Data}, #state{socket=Socket, server = Server} = State) ->
    {Host, Path, Origin, Body} = websocket:handshake(binary_to_list(Data)),
	Response = handshake_response(Body, Host, Path, Origin),
	catch gen_tcp:send(Socket, Response),
	{next_state, 'WAIT_FOR_DATA', State}.

handshake_response(Body, Host, Path, Origin) ->
	["HTTP/1.1 101 WebSocket Protocol Handshake\r\n",
		"Upgrade: WebSocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Origin: ", Origin, "\r\n",
		"Sec-WebSocket-Location: ", "ws://", lists:concat([Host, Path]), "\r\n\r\n",
		Body
	].

parse_data([0 | R]) ->
	parse_data(R, []).
parse_data([255], Body)->
	List = lists:reverse(Body),
	DList = base64:mime_decode(List),
	DList;
parse_data([E|R], Body)->
	L = [E|Body],
	parse_data(R, L).

decode_data(<<>>, Dest) -> Dest;
decode_data(<<B:?BYTE, R/binary>>, Dest) when B band 16#E0 =:= 16#E0 ->
	<<B2:?BYTE, B3:?BYTE, R2/binary>> = R,
	decode_data(R2, <<Dest/binary, B:?BYTE, B2:?BYTE, B3:?BYTE>>);
decode_data(<<B:?BYTE, R/binary>>, Dest) when B band 16#C0 =:= 16#C0 ->
	T1 = (B band 16#1F) bsl 6,
	<<T2:?BYTE, R2/binary>> = R,
	T3 = T2 band 16#3F,
	B2 = T1 + T3,
	if (B2 band 16#FF00) /= 0 ->
		Byte = (B2 band 16#FF00) bsr 8,
		Byte2 = B2 band 16#00FF,
		decode_data(R2, <<Dest/binary, Byte:?BYTE, Byte2:?BYTE>>);
	true ->
		Byte2 = B2 band 16#00FF,
		decode_data(R2, <<Dest/binary, Byte2:?BYTE>>)
	end;
decode_data(<<B:?BYTE, R/binary>>, Dest) ->
	decode_data(R, <<Dest/binary, B:?BYTE>>).


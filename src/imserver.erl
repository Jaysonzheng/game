%% Author: xiaoshengaya
%% Created: 2009-12-31
%% Description: chatserver
-module(imserver).

-include("common.hrl").
-include("protocol.hrl").

-export([init/0, start/0, handle_accept/1, handle_read/2, handle_close/1, get_room_conf/1]).


%% {ok,Server}=reltool:start_server([{config,"chat.config"}]). 
%% reltool:create_target(Server, "target").
%% erl -pa ./ebin/ -noshell +P 102400 -sname chatapp +K true +S 2 -smp -s chatserver
%% /usr/local/lib/erlang/lib/observer-0.9.8.1/priv/bin/etop -node chatapp@local5
%% start the chat server

%%chatserver实现4个接口,init, handle_accept, handle_read, handle_close

start() ->
	tcp_server_app:start(imserver).

init() ->
	chatets:init().

handle_accept(_Sock) ->
	io:format("handle accept\n"),
	ok.

handle_read(Sock, Data) ->
	case protocol:parse_header(Data) of
	{error, _} ->
		io:format("parse packet error~n"),
		user_logout(Sock),
		gen_tcp:close(Sock);
				
	{ok, CmdType, Body} ->
		case parse_cmd(Sock, Data, CmdType, Body) of
		cmd_error ->
			user_logout(Sock),
			gen_tcp:close(Sock);
		_ ->
			ok
		end	
end.

handle_close(Sock) ->
%%	io:format("chatserver handle_close\n"),
	user_logout(Sock).

%%process client cmd
parse_cmd(Sock, Data, CmdType, Body) ->
	NewBody = <<CmdType:?SHORT, Body/binary>>, 
	case packet:read(NewBody) of
		{?CMD_CLIENT_LOGIN, UserId, IsOnline, FriendList} -> user_login(Sock, UserId, IsOnline, FriendList);
		
		{?CMD_CLIENT_LOGOUT} -> user_logout(Sock);
		
		{?CMD_CLIENT_CHAT, FriendId} -> user_chat(FriendId, Data);
		
		{?CMD_CLIENT_ENTER_ROOM, RoomId} -> user_enter_room(Sock, RoomId, Data);
		
		{?CMD_CLIENT_LEAVE_ROOM} -> user_leave_room(Sock, Data);
		
		{?CMD_CLIENT_BC_ALL_USERS} -> user_bc_all_users(Data);
		
        {?CMD_CLIENT_BC_FRIENDS} -> user_bc_friends(Sock, Data);
		
		{?CMD_CLIENT_SET_STATUS, Status} -> user_set_status(Sock, Status);
	
	    {?CMD_CLIENT_GET_USER_COUNT} -> get_online_count(Sock);
		
		{?CLIENT_CMD_JOIN_GAME, Level} -> user_join_game(Sock, Level);
		
		_ -> 
			error_logger:error_msg("unkown cmd type~n"),
			cmd_error
	end.	

%% 用户登录
user_login(Sock, UserId, IsOnline, FriendIdList) ->
	case chatets:add_user(UserId, Sock, IsOnline, FriendIdList) of
		{kickother, KickSock} ->							%% 踢掉该账号的另一账户 ,不用广播好友
			KickPacket = protocol:build_packet(?SERVER_CMD_KICK_OUT, <<>>),
			send_packet(KickSock, KickPacket),
			gen_tcp:close(KickSock);
			
		newuser ->
			Body = <<UserId:?DWORD>>,
			Data = protocol:build_packet(?SERVER_BC_USER_LOGIN, Body),
			if 
			IsOnline == 1 ->	%%如果显身登陆,广播好友,否则只获取在线好友列表返回
				broadcast_friends(UserId, Data);	%%broadcast other user that new user login
			true ->
				ok
			end
	end,

	FriendList = get_online_show_friend_id_list(UserId),
	%%io:format("get_online_show_friend_id_list...~p~n", [FriendList]),
	send_login_success(Sock, FriendList).					%%回应用户登录成功,返回好友Id列表

%% 用户注销
user_logout(Sock) ->
%% 	gen_server:cast(?MODULE, {connect_close, self()}),
	case chatets:get_uid_by_sock(Sock) of
		{ok,UserId} -> 
			Body = <<UserId:?DWORD>>,
			Data = protocol:build_packet(?SERVER_BC_USER_LOGOUT, Body),
			broadcast_friends(UserId, Data),
			chatets:del_user(Sock);
		{error, not_found} ->
			ok
	end.

%% 用户私聊
user_chat(FriendId, Data) ->
	case chatets:get_socket_by_uid(FriendId) of
		{ok, FriendSock}   -> send_packet(FriendSock, Data);
		{error, not_found} -> not_found
	end.

%% 用户进入房间
user_enter_room(Sock, RoomId, Data) ->
	chatets:user_enter_room(Sock, RoomId),
	user_bc_friends(Sock, Data).

%% 用户退出房间
user_leave_room(Sock, Data) ->
	chatets:user_leave_room(Sock),
	user_bc_friends(Sock, Data).

%% 广播所有用户
user_bc_all_users(Data) ->    
	io:format("bc all user~n"),
	UserList = chatets:get_all_users(),
	broadcast_alluser(UserList, Data).

%% 广播好友
user_bc_friends(Sock, Data) ->
	{ok,UserId} = chatets:get_uid_by_sock(Sock),
	UserList = chatets:get_online_friends(UserId),
	broadcast_alluser(UserList, Data).

%% 用户设置状态,广播用户上线或隐身,发送登陆或注销命令
user_set_status(Sock, Status) ->
	%%io:format("Status=~p~n", [Status]),
	chatets:user_set_status(Sock, Status),
	{ok,UserId} = chatets:get_uid_by_sock(Sock),
	Body = <<UserId:?DWORD>>,
	if 
	Status == 1 ->
		Data = protocol:build_packet(?SERVER_BC_USER_LOGIN, Body);
	true ->
		Data = protocol:build_packet(?SERVER_BC_USER_LOGOUT, Body)
	end,	
	broadcast_friends(UserId, Data).

%% 获取在线用户数
get_online_count(Sock) ->
    Count = chatets:get_all_user_count(),
    Body = <<Count:?DWORD>>,
    Data = protocol:build_packet(?SERVER_CMD_RES_USER_COUNT, Body),
    send_packet(Sock, Data).

%% 用户请求加入游戏
user_join_game(Sock, Level) ->
	{Level, _Base, Require, _Expand, _OutTime} = get_room_conf(Level),
	case chatets:get_user_by_sock(Sock) of 
		{ok, User} ->
			#player{core = Core} = User,
			#player_data{money = Money} = Core,
			if Require > Money ->
				Err_not_enough_money = 1,
				Body = <<Err_not_enough_money:?SHORT>>,
				Data = protocol:build_packet(<<?SERVER_CMD_JOIN_GAME_FAILED:?SHORT, Body>>),
				send_packet(Sock, Data),
				not_enough_money;
			true ->
				case game_room_db:get_game_room(Level) of
				{ok, RoomId, PId} ->
					game:join_room(User, RoomId);
				{error, _Reason} ->
					Err_not_find_room = 2,
					Body = <<Err_not_find_room:?SHORT>>,
					Data = protocol:build_packet(<<?SERVER_CMD_JOIN_GAME_FAILED:?SHORT, Body>>),
					send_packet(Sock, Data)
				end
			end;
		{error, not_found} -> not_found
	end.

%% 广播所有用户
%% create many processes to send data
broadcast_alluser(UserList, Data) ->
	SendData = fun(User) ->
%%		spawn(fun()-> 
    		send_packet(User#player.socket, Data) 
%%		end)			 
  	end,
  	lists:foreach(SendData, UserList).

%% 广播好友
broadcast_friends(UserId, Data) ->
	FriendList = chatets:get_online_friends(UserId),
	%%io:format("UserId=~w,UserList=~w~n", [UserId, FriendList]),

	SendData = fun(User) ->
%%		spawn(fun()-> 
			send_packet(User#player.socket, Data)
%%    	end)			 
  	end,
  	lists:foreach(SendData, FriendList),
	[Id || {_Player, Id, _Socket, _Friends, _Status,  _RoomId, _Time}<-FriendList]. %%取出好友id列表做返回值

%%获取所有好友的id列表,包括隐身好友
get_online_friend_id_list(UserId) ->
	FriendList = chatets:get_online_friends(UserId),
	[Id || {_Player, Id, _Socket, _Friends, _Status, _RoomId, _Time}<-FriendList]. %%取出好友id列表做返回值

%%获取所有好友的id列表,不包括隐身好友
get_online_show_friend_id_list(UserId) ->
	FriendList = chatets:get_online_show_friends(UserId),
	[Id || {_Player, Id, _Socket, _Friends, _Status, _RoomId, _Time}<-FriendList]. %%取出好友id列表做返回值

send_login_success(Sock, FriendList) ->
	OnlineFriendCount = length(FriendList),			%回应用户登录成功,并返回在线好友Id列表
	Bin = <<OnlineFriendCount:?LONG>>,
	RetPacket = protocol:write_int_binary(Bin, FriendList),
	%%io:format("FriendList = ~w, RetPacket = ~w ,OnlineFriendCount = ~w~n", [FriendList, RetPacket, OnlineFriendCount]),
	RetData = protocol:build_packet(?SERVER_CMD_LOGINSUCCESS, RetPacket),
	send_packet(Sock, RetData).		%echo the new user login success

send_packet(Socket, Packet) ->
%%	gen_tcp:send(Sock, Packet).
	case catch gen_tcp:send(Socket, Packet) of
        ok ->
            ok;
        {error, closed} ->
            ok;
        {error,econnaborted} ->
            ok;
        Any ->
            error_logger:error_report([
                                       {message, "gen_tcp:send error"},
                                       {module, ?MODULE},
                                       {line, ?LINE},
                                       {socket, Socket},
                                       {port_info, erlang:port_info(Socket, connected)},
                                       {bin, Packet},
                                       {error, Any}
                                      ])
    end.

get_room_conf(BaseChip) ->
	%% level, basechip, require money, expand, outcard_time
	RoomConf = [
		{level, basechip, require_money, expand, outcard_time},
		{1, 20,  200,   10, 30},
		{2, 50,  1000,  10, 20},
		{3, 100, 5000,  10, 20},
		{4, 200, 10000, 10, 20}
	],
	get_room_conf(BaseChip, RoomConf).

get_room_conf(BaseChip, [{Level, Base, Require, Expand, OutTime} | R]) when Base =:= BaseChip ->
	{Level, Base, Require, Expand, OutTime};
get_room_conf(BaseChip, [{Level, Base, Require, Expand, OutTime} | R]) when Base /= BaseChip ->
	get_room_conf(BaseChip, R);
get_room_conf(BaseChip, RoomConf) when RoomConf =:= [] ->
	DefaultConf = {1, 20,  200, 10, 30}.


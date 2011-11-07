%%% -------------------------------------------------------------------
%%% Author  : Administrator
%%% Description :
%%%
%%% Created : 2010-1-20
%%% -------------------------------------------------------------------
-module(chatets).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/0, 
		 add_user/4,
		 del_user/1,
		 get_online_friends/1,
		 get_online_show_friends/1,
		 get_socket_by_uid/1,
		 get_uid_by_sock/1, 
		 get_user_by_uid/1,
		 get_user_by_sock/1,
		 get_all_users/0,
	     get_all_user_count/0,
		 user_enter_room/2, 
		 user_leave_room/1,
		 user_set_status/2
	 ]).


-define(TABLE, member).

%% ====================================================================
%% External functions
%% ====================================================================
init() ->
	ets:new(?TABLE, [public, set, named_table, {keypos, #player.id}]).

%% user connected, add user
add_user(UserId, Socket, Status, FriendList) ->
	{ok, {IP, _Port}} = inet:peername(Socket),
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[User] -> #player{socket=KickSock} = User;
		[] -> KickSock = null
	end,
	
	Player = #player{
		id = UserId, 
		socket = Socket,
		friends = FriendList,
		status = Status, 
		roomid = 0,
		time = time()        
	},
	ets:insert(?TABLE,Player),

	%%io:format("current connect:~p,~p~n", [UserId, length(NewUserList)]),
	case KickSock of
		null ->
	 	   	newuser;
		_ ->
			{kickother, KickSock}
	end.

%% use disconnect, delete user 
del_user(Socket) ->
	case ets:match_object(?TABLE, #player{socket = Socket, _ = '_'}) of 
		[{player, Id, _Socket, _Friends, _Status, _Roomid, _Time, _Ip}] -> 
			ets:delete(?TABLE, Id),
 	   		ok;	
		
		[] -> 
			{error, not_found}
	end.

%% get user's online friend id list by user id
%% 获取用户所有在线包括隐身好友id列表
get_online_friends(UserId) ->
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[User] -> 
			#player{friends = FriendList} = User,
			OnlineList = get_online_userlist(FriendList, []),
			%%io:format("UserList:~w,AllFriendList:~w,OnlineList:~w~n", [UserList, AllFriendList, OnlineList]),
			OnlineList;
		[] ->
			{error, not_found}
	end.	

%% get user's online friend id list by user id
%% 获取用户所有在线不隐身的好友id列表
get_online_show_friends(UserId) ->
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[User] -> 
			#player{friends = FriendList} = User,
			OnlineList = get_online_show_userlist(FriendList, []),
			%%io:format("AllFriendList:~w,OnlineList:~w~n", [FriendList, OnlineList]),
			OnlineList;
		[] ->
			{error, not_found}
	end.	

%% get socket by user id
get_socket_by_uid(UserId) -> 
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[User] -> 
			#player{socket = Socket} = User,
			{ok, Socket};
		[] -> 
			{error, not_found}
    end.


%% get user id by socket
get_uid_by_sock(Sock) ->
	case ets:match_object(?TABLE, #player{socket = Sock, _ = '_'}) of 
		[User] -> 
			#player{id = Id} = User,
 	   		{ok, Id};	
		[] -> 
			{error, not_found}
	end.	

%% get user by use id
get_user_by_uid(UserId) ->
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[Player] -> 
			{ok, Player};
		[] -> 
			{error, not_found}
	end.	

get_user_by_sock(Sock) ->
	case ets:match_object(?TABLE, #player{socket = Sock, _ = '_'}) of 
		[User] -> 
 	   		{ok, User};	
		[] -> 
			{error, not_found}
	end.	

%% user chat
get_all_users() ->
	UserList = ets:match_object(?TABLE, '$1'),
    UserList.

get_all_user_count() ->
    UserList = get_all_users(),
    length(UserList).
    
%% 用户进入房间,更新Roomid
user_enter_room(Sock, RoomId) ->
	case ets:match_object(?TABLE, #player{socket = Sock, _ = '_'}) of 
		[User] -> 
			NewUser = User#player{
				roomid = RoomId,
				time = time()
			},
			ets:insert(?TABLE, NewUser),			
 	   		ok;	
		[] -> 
			{error, not_found}
	end.	

%% 用户退出房间,更新RoomId为0
user_leave_room(Sock) ->
	case ets:match_object(?TABLE, #player{socket = Sock, _ = '_'}) of 
		[User] -> 
			NewUser = User#player{
				roomid = 0,
				time = time()
			},
			ets:insert(?TABLE,NewUser),			
 	   		ok;	
		[] -> 
			{error, not_found}
	end.

%% 用户更新状态, 0为隐身,1为在线
user_set_status(Sock, Status) ->
	case ets:match_object(?TABLE, #player{socket = Sock, _ = '_'}) of 
		[User] -> 
			NewUser = User#player{
				status = Status,
				time = time()
			},
			ets:insert(?TABLE,NewUser),			
 	   		ok;	
		[] -> 
			{error, not_found}
	end.
	
%% get client list count
%%get_status() ->
%%    {ok, length(UserList)}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

get_online_userlist([], OnlineList) -> OnlineList;
get_online_userlist(FriendIdList, OnlineList) ->
	[UserId|RestList] = FriendIdList,	
	case ets:lookup(?TABLE, UserId) of		%% 查找该用户是否已在线
		[User] -> 
			NewOnlineList = [User|OnlineList];
		[] -> 
			NewOnlineList = OnlineList
	end,
	get_online_userlist(RestList, NewOnlineList).

get_online_show_userlist([], OnlineShowList) -> OnlineShowList;
get_online_show_userlist(FriendIdList, OnlineShowList) ->
	[UserId|RestList] = FriendIdList,%%{player, UserId, '_', '_', 1, '_', '_'}
	case ets:match_object(?TABLE, #player{id = UserId, status = 1, _ = '_'}) of 
  		[User] -> 
			NewOnlineShowList = [User|OnlineShowList];
		[] -> 
			NewOnlineShowList = OnlineShowList
  	end,
	get_online_show_userlist(RestList, NewOnlineShowList).
	

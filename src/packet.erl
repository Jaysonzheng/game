%% Author: xiaoshengaya
%% Created: 2010-1-6
%% Description: 
-module(packet).

%%
%% Include files
%%
-include("protocol.hrl").

%%
%% Exported Functions
%%
-export([read/1]).
	
read(<<?CMD_CLIENT_LOGIN:?SHORT, UserId:?LONG, IsOnline:?SHORT, Bin/binary>>) ->
    %{Nick, Bin1} = protocol:read_string(Bin),
    %{Password, _} = protocol:read_string(Bin1),
    %{?CMD_CLIENT_LOGIN, UserId, Nick, Password};
	%%List = [],
	
	FriendList = protocol:read_int_list(Bin, []),
	%io:format("friendList = ~w~n", [FriendList]),
	{?CMD_CLIENT_LOGIN, UserId, IsOnline, FriendList};
	
read(<<?CMD_CLIENT_LOGOUT:?SHORT>>) ->
	{?CMD_CLIENT_LOGOUT};

read(<<?CMD_CLIENT_CHAT:?SHORT, _User1Id:?LONG, User2Id:?LONG, _Bin/binary>>) ->
	%%io:format("user chat friend id = ~w~n", [UserId]),
	{?CMD_CLIENT_CHAT, User2Id};

read(<<?CMD_CLIENT_BC_FRIENDS:?SHORT, _Bin/binary>>) ->
    %%{Content, _} = protocol:read_string(Bin),
	{?CMD_CLIENT_BC_FRIENDS};

read(<<?CMD_CLIENT_BC_ALL_USERS:?SHORT, _Bin/binary>>) ->
    %%{Content, _} = protocol:read_string(Bin),
	{?CMD_CLIENT_BC_ALL_USERS};

read(<<?CMD_CLIENT_ENTER_ROOM:?SHORT, _UserId:?LONG, RoomId:?LONG>>) ->
	{?CMD_CLIENT_ENTER_ROOM, RoomId};

read(<<?CMD_CLIENT_LEAVE_ROOM:?SHORT, _UserId:?LONG>>) ->
	{?CMD_CLIENT_LEAVE_ROOM};

read(<<?CMD_CLIENT_SET_STATUS:?SHORT, Status:?SHORT>>) ->
	{?CMD_CLIENT_SET_STATUS, Status};

read(<<?CMD_CLIENT_GET_USER_COUNT:?SHORT>>) ->
        {?CMD_CLIENT_GET_USER_COUNT};

read(<<?CLIENT_CMD_JOIN_GAME:?SHORT, Level:?LONG>>) ->
	{?CLIENT_CMD_JOIN_GAME, Level};


read(Bin) when is_binary(Bin) ->
	error_logger:error_info("xxxx~n"),			
   	Bin.

%%
%% Local Functions
%%


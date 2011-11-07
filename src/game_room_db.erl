%%% -------------------------------------------------------------------
%%% Author  : zrs
%%% Description :
%%%
%%% Created : 2011-11-06
%%% -------------------------------------------------------------------
-module(game_room_db).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

 
%% --------------------------------------------------------------------
%% External exports
-export([init/0,  
		 get_game_room_by_roomid/1,
		 get_game_room/1,
		 get_game_room/2,
		 init_game_room_list/0,
		 add_game_room/0,
		 add_game_room/1,
		 update_game_room/4,
		 delete_game_room/1,
		 get_all/0,
		 test/0
		]).

%% ====================================================================
%% External functions
%% ====================================================================
init() -> 
	ets:new(?GAME_ROOM_DB, [public, set, named_table, {keypos, #roominfo.roomid}]),			
	ets:new(?GAME_ROOM_ID_DB, [public, set, named_table]),
	ets:insert(?GAME_ROOM_ID_DB, {max_id, 100}).

init_game_room_list() -> 
	ets:delete_all_objects(?GAME_ROOM_DB),
	add_game_room(),
	add_game_room(),
	add_game_room(),
	add_game_room(),
	add_game_room().

get_game_room_by_roomid(RoomId)->
	case ets:lookup(?GAME_ROOM_DB, RoomId) of
		[Room] -> #roominfo{pid = Pid} = Room;
		[] -> not_found
	end.

get_game_room(Level) ->	
	%%RoomUserCount = tcp_server_app:get_app_env(room_user_count, 100),
	get_game_room(Level, 2).

get_game_room(Level, -1) -> 
	case add_game_room(Level) of
	{ok, RoomId, PId} ->
		{ok, RoomId, PId};
	{error, Reason} ->
		{error, Reason}
	end;
get_game_room(Level, UserCount) ->
	case ets:match(?GAME_ROOM_DB, {roominfo, '$0', '$1', Level, UserCount}) of	
		[[RoomId, PId] | _Rest] -> 
			?D("get_game_room find room RoomId:~p, PId:~p, UserCount:~p~n", [RoomId, PId, UserCount]),
			update_game_room(RoomId, PId, Level, UserCount+1),		
 	   		{ok, RoomId, PId};
		[] -> 
			get_game_room(Level, UserCount - 1)	
	end.	

add_game_room()->
	add_game_room(1).

add_game_room(Level) ->
	{ok, RoomId} = inc_max_room_id(),		
	{ok, Pid} = game:start_link(RoomId), 
	ets:insert(?GAME_ROOM_DB, {roominfo, RoomId, Pid, Level, 0}),
	{ok, RoomId, Pid}.

update_game_room(RoomId, PId, Level, UserCount) ->
	ets:insert(?GAME_ROOM_DB, {roominfo, RoomId, PId, Level, UserCount}).

delete_game_room(PId) ->
	ets:match_delete(?GAME_ROOM_DB, {roominfo, '_', PId, '_', '_'}).
	
get_all() ->
	All = ets:match(?GAME_ROOM_DB, '$1').

inc_max_room_id () ->
	[{max_id, Id}] = ets:lookup(?GAME_ROOM_ID_DB, max_id), 
	NewId =  Id + 1,
	ets:insert(?GAME_ROOM_ID_DB, {max_id, NewId}), 
	{ok, NewId}.
	
test() ->
	%%init(),
	ets:insert(?GAME_ROOM_DB, {roominfo, 1, 1, 1, 1}),
	ets:insert(?GAME_ROOM_DB, {roominfo, 2, 1, 2, 2}),
	ets:insert(?GAME_ROOM_DB, {roominfo, 3, 1, 1, 3}),
 	{ok, RoomId, PId} = get_game_room(1),
 	?D("get game room:PId:~p, RoomId:~p~n", [PId, RoomId]),	
 	update_game_room(1, 1, 1, 2),
 	{ok, RoomId2, PId2} = get_game_room(1),
 	?D("get game room:PId:~p, RoomId:~p~n", [PId2, RoomId2]),	
% 	init_game_room_list(2, 1, [{1,2}, {2,2}, {3,1}]),
 	{ok, RoomId3, PId3} = get_game_room(1),
 	io:format("get game room:PId:~p, RoomId:~p~n", [PId3, RoomId3]),	
%% 	game_room_db:get_game_room(1),	
	ok.


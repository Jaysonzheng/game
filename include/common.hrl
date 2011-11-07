%% Author: xiaoshengaya
%% Created: 2009-12-31
%% Description: TODO: Add description to server
-record(server_state, {
			port,			% listen port
			ip=any,			% ip
			lsocket=null,	% listen socket
			conn=0,			% curent connect
			maxconn			% max connect
			}).	

-record(player_data, {
		money = 0, 
		exp = 0, 
		win_times = 0, 
		lose_times = 0
		}).

-record(player, {
			id,				% user Id
			socket,			% socket handle
			friends,		% friend id list
			status,			% status, 0:offline, 1:online
			roomid = 0,		% room id
			time = none,	% active time
		    game = none,	% game pid
			seatid = -1,	% seat id
			userinfo = "",	% user info
			core = #player_data{} %core data, save in db
			}).

-record(roominfo,{
			  roomid,		%% room id
			  pid,			%% process id
			  level,		%% level
			  usercount		%% user count
			  }).

-define (IMSG(X), error_logger:info_msg("~p:~p  ~p~n",[?MODULE, ?LINE, X])). 
-define (IREPORT(X), error_logger:info_report([{module, ?MODULE}, {line, ?LINE}|X])). 
-define (EMSG(X), error_logger:error_msg("~p:~p  ~p~n",[?MODULE, ?LINE, X])). 
-define (EREPORT(X), error_logger:error_report([{module, ?MODULE}, {line, ?LINE}|X])). 

-define(D(Format, Args), io:format(Format, Args)).
-define(DD(Format), io:format(Format)).

-define(GAME_ROOM_DB, game_room_db).
-define(GAME_ROOM_ID_DB, game_room_id_db).



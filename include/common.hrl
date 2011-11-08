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

-record(player_data, {		% 玩家核心数据
		money = 0,			% money	 
		exp = 0,			% 经验值
		win_times = 0,		% 赢次数
		lose_times = 0		% 输次数
		}).

-record(player_game_data, { % 用户玩牌时候数据
		hand_cards = [],	% 手上剩余牌
		is_ai = false,		% 是否机器人	
		out_times = 0		% 出牌次数
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
			userinfo = "user",	% user info
			core = #player_data{}, % core data, save in db
			game_data = #player_game_data{} % game process data
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

-ifdef(NDEBUG).
-define(DEBUG(F, D), ok).
-else.
-define(DEBUG(F, D), io:format("[~p:~p]----"++F, [?MODULE, ?LINE]++D)).
-endif.

-define(GAME_ROOM_DB, game_room_db).
-define(GAME_ROOM_ID_DB, game_room_id_db).



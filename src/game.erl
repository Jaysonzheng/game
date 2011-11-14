-module(game).

-behaviour(gen_fsm).


%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([join_room/2, leave_room/2, call_landlord/3]).

-export([
	'STOP_GAME'/2,     	%% stop game
    'WAIT_START'/2    	%% wait for game start
]).

-include("common.hrl").
-include("protocol.hrl").

-define(HAND_CARD_COUNT, 17).

-record(game_conf, {
	base_chips = 0,
	require_chips = 0,
	landlord_time = 10,
	outcard_time = 20
}).

-record(game_state , {
	conf = #game_conf{},			% 游戏配置
	user_list = [],					% 用户列表
	spring = 1,						% 是否春天局
	total_times = 0,				% 倍数
	landlord_user = 0,				% 地主
	backcards = [],					% 底牌
	
	is_calling = false,				% 开始
	start_call_user = 1,			% 开始叫地主玩家
	last_call_user = 0,				% 最后一个叫地主的玩家

	cur_op_user = -1,				% 当前操作（叫分，出牌）玩家
	turn_winner = 0,				% 当前回合的赢家
	turn_cards = [],				% 当前回合扑克牌

	hand_cards = [[],[],[]],		% 手上剩余牌
	is_ai = [false, false, false],	% 是否机器人
%%	is_can_call = [true, true, true],	% 是否可以叫
	out_times = [0, 0, 0]			% 出牌次数
}).

-define(TIMEOUT, 2000).

start_link(RoomId) ->
	RId = erlang:integer_to_list(RoomId),
	Name = "room" ++ RId,
	PName = erlang:list_to_atom(Name),
    gen_fsm:start_link({local, PName}, ?MODULE, [], []).

init([]) ->
    {ok, 'START_CALL_LANDLORD', #game_state{}}.

join_room(User, PId) ->
	gen_fsm:send_event(PId, {join_room, User, PId}).

leave_room(User, PId)->
	gen_fsm:send_event(PId, {leave_room, User}).

call_landlord(User, PId, Action) ->
	gen_fsm:send_event(PId, {call_landlord, User, Action}).

'STOP_GAME'({join_room, User, PId}, State) ->
	?D("Somebody come in!~p~n", [User]),
	#game_state{user_list = UserList, conf = GameConf} = State, 
	NewUserList = UserList ++ [User],	
	UserCount = length(NewUserList),
	chatets:set_user_gamedata(User, PId, UserCount),
	if UserCount =:= 3 ->		%%凑够三个人，准备开始游戏
		%% ?DD("ready start game~n"),
		%%准备开始
		User1 = lists:nth(1, NewUserList),
		User2 = lists:nth(2, NewUserList),
		User3 = lists:nth(3, NewUserList),
		bc_user_wait_start(GameConf, User1, User2, User3),

		%% 发牌
		{Cards1, Cards2, Cards3, BackCard} = card_logic:deal_card(?HAND_CARD_COUNT),
		NewState = State#game_state{user_list = NewUserList, 
									backcards = BackCard, 
									hand_cards = [Cards1, Cards2, Cards3]
								},
		send_player_cards(User1, Cards1),
		send_player_cards(User2, Cards2),
		send_player_cards(User3, Cards3),
		
		start_timer(?TIMEOUT, "WaitStart"),
		{next_state, 'WAIT_START', NewState};
	true ->
		NewState = State#game_state{user_list = NewUserList},
		{next_state, 'STOP_GAME', NewState}
	end;
'STOP_GAME'({leave_room, User}, State) ->
	#game_state{user_list = UserList} = State, 
	NewUserList = UserList -- [User],
	NewState = State#game_state{user_list = NewUserList},
	chatets:set_user_gamedata(User, none),
	{next_state, 'STOP_GAME', NewState};
'STOP_GAME'(_Other, State) ->
	?DEBUG("STOP_GAME~n", []),
	{next_state, 'STOP_GAME', State}.

'WAIT_START'({timeout, _Ref, _Msg}, State) ->
	#game_state{user_list = UserList} = State, 
	%%call_landlord(0, 1, 0, UserList),
	NewState = State#game_state{cur_op_user = 1},
	
	{next_state, 'START_CALL_LANDLORD', NewState};
'WAIT_START'(_Other, State) ->
	{next_state, 'WAIT_START', State}.

'START_CALL_LANDLORD'({call_landlord, User, Action}, #game_state{cur_op_user = OpUser} = State) when User /= OpUser-> %%when User#player.id /= OpUser -> 
	{next_state, 'START_CALL_LANDLORD', State};

'START_CALL_LANDLORD'({call_landlord, User, Action}, #game_state{cur_op_user = OpUser, 
																is_calling = IsCalling, 
																last_call_user = LastUser
																} = State)
														when Action == 0 ->
	if 
	OpUser == 3 andalso IsCalling == false ->	%% 三个玩家都不叫，要重新发牌
		{next_state, 'START_CALL_LANDLORD', State};
	IsCalling == false ->						%% 还没开始叫，置开始叫的玩家为下一个
		NewState = State#game_state{start_call_user = OpUser + 1, cur_op_user = OpUser + 1},
		{next_state, 'START_CALL_LANDLORD', NewState};
	OpUser == 3 ->								%% 第三个玩家没叫，但此前已经有叫过了
		NewState = State#game_state{cur_op_user = LastUser},
		{next_state, 'START_CALL_LANDLORD', NewState};
	OpUser == LastUser ->						%% 最后一个玩家不叫，就开始游戏了
		NewState = State#game_state{},
		{next_state, 'START_GAME', State};
	true ->										%% 轮到下一家抢	
		NewState = State#game_state{cur_op_user =  OpUser + 1},
		{next_state, 'START_CALL_LANDLORD', NewState}
	end;

'START_CALL_LANDLORD'({call_landlord, User, Action}, #game_state{cur_op_user = OpUser, 
																is_calling = IsCalling, 
																total_times = TotalTimes, 
																start_call_user = Start_User, 
																last_call_user = LastUser} = State)
														when Action == 1 ->
	if IsCalling == true ->
		Times = TotalTimes * 2,
		if  OpUser == LastUser ->	%%最后一个抢地主，开始游戏，否则继续抢
			{next_state, 'START_GAME', State#game_state{landlord_user = OpUser, total_times = Times}};
		true ->
			{next_state, 'START_CALL_LANDLORD', State#game_state{total_times = Times, cur_op_user = OpUser + 1}}
		end;		
	true ->
		NewState = State#game_state{total_times = 3, start_call_user = OpUser, last_call_user = OpUser}, 
		if OpUser == 3 ->			%%从C才开始叫地主，直接开始游戏
			{next_state, 'START_GAME', NewState};
		true ->
			{next_state, 'START_CALL_LANDLORD', NewState}
		end
	end.

bc_user_wait_start(GameConf, User1, User2, User3) ->
	CallLandlordTime = GameConf#game_conf.landlord_time,
	OutCardTime = GameConf#game_conf.outcard_time,

	Money1 = (User1#player.core)#player_data.money, 
	Money2 = (User2#player.core)#player_data.money, 
	Money3 = (User3#player.core)#player_data.money, 
	Bin1 = protocol:write_string(<<>>, User1#player.userinfo),
	Bin2 = protocol:write_string(Bin1, User2#player.userinfo),
	Bin3 = protocol:write_string(Bin2, User3#player.userinfo),
	Bin = <<CallLandlordTime:?SHORT, OutCardTime:?SHORT, Money1:?LONG, Money2:?LONG, Money3:?LONG, Bin3/binary>>, 
	Packet = protocol:build_packet(?SERVER_CMD_BC_WAIT_START, Bin), 
	bc_user_packet([User1, User2, User3], Packet),
	ok.

bc_user_call_landlord(PreCallUser, PreCallScore, NextCallUser,  UserList) ->
	Packet = protocol:build_packet(?SERVER_CMD_BC_CALL_LANDLORD, <<PreCallUser:?LONG, PreCallScore:?LONG, NextCallUser:?LONG>>),
	bc_user_packet(UserList, Packet),
	ok.

bc_user_packet([], _Packet) -> ok;
bc_user_packet([User | Rest], Packet) ->
	imserver:send_packet(User#player.socket, Packet),
	bc_user_packet(Rest, Packet).

send_player_cards(User, Cards) ->
	Bin = protocol:write_byte_list(<<>>, Cards),
	Packet = protocol:build_packet(?SERVER_CMD_DEAL_CARD, Bin), 
	imserver:send_packet(User#player.socket, Packet).

start_timer(Time, Msg) ->
	gen_fsm:start_timer(Time, Msg).

%% gen_fsm template 
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


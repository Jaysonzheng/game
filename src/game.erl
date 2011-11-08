-module(game).

-behaviour(gen_fsm).


%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([join_room/2, leave_room/2]).

-export([
    'WAIT_START'/2,    	%% wait for game start
	'STOP_GAME'/2      	%% stop game
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
	bomb_times = 0,					% 炸弹倍数
	cur_landtimes = 0,				% 当前叫地主的倍数
	turn_winner = 0,				% 当前回合的赢家
	turn_cards = [],				% 当前回合扑克牌
	cur_outcard_user = 0,			% 当前出牌玩家
	landlord_user = 0,				% 地主
	backcards = [],					% 底牌

	hand_cards = [[],[],[]],		% 手上剩余牌
	is_ai = [false, false, false],	% 是否机器人	
	out_times = [0, 0, 0]			% 出牌次数
}).

start_link(RoomId) ->
	RId = erlang:integer_to_list(RoomId),
	Name = "room" ++ RId,
	PName = erlang:list_to_atom(Name),
    gen_fsm:start_link({local, PName}, ?MODULE, [], []).

init([]) ->
    {ok, 'STOP_GAME', #game_state{}}.

join_room(User, PId) ->
	gen_fsm:send_event(PId, {join_room, User, PId}).

leave_room(User,PId)->
	gen_fsm:send_event(PId, {leave_room, User}).

'WAIT_START'(_Other, State) ->
	{next_state, 'WAIT_START', State}.

'STOP_GAME'({join_room, User, PId}, State) ->
	?D("Somebody come in!~p~n", [User]),
	chatets:set_user_gamepid(User, PId),
	#game_state{user_list = UserList, conf = GameConf} = State, 
	NewUserList = UserList ++ [User],	
	if length(NewUserList) =:= 3 ->		%%凑够三个人，准备开始游戏
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
		

		{next_state, 'WAIT_START', NewState, 2000};
	true ->
		NewState = State#game_state{user_list = NewUserList},
		{next_state, 'STOP_GAME', NewState}
	end;
'STOP_GAME'({leave_room, User}, State) ->
	#game_state{user_list = UserList} = State, 
	NewUserList = UserList -- [User],
	NewState = State#game_state{user_list = NewUserList},
	chatets:set_user_gamepid(User, none),
	{next_state, 'STOP_GAME', NewState};
'STOP_GAME'(_Other, State) ->
	{next_state, 'STOP_GAME', State}.

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

bc_user_packet([], _Packet) -> ok;
bc_user_packet([User | Rest], Packet) ->
	imserver:send_packet(User#player.socket, Packet),
	bc_user_packet(Rest, Packet).

send_player_cards(User, Cards) ->
	Bin = protocol:write_byte_list(<<>>, Cards),
	Packet = protocol:build_packet(?SERVER_CMD_DEAL_CARD, Bin), 
	imserver:send_packet(User#player.socket, Packet).

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


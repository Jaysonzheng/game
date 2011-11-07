-module(game).

-behaviour(gen_fsm).


%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([
    'WAIT_START'/2,    	%% wait for game start
	'STOP_GAME'/2      	%% stop game
]).

-include("common.hrl").
-include("protocol.hrl").

-record(game_conf, {
	base_chips = 0,
	require_chips = 0,
	landlord_time = 10,
	outcard_time = 20
}).

-record(game_state , {
	spring = 1,
	bomb_times = 0,
	user_list = [],
	cur_landtimes = 0,
	conf = #game_conf{}
}).

start_link(RoomId) ->
	RId = erlang:integer_to_list(RoomId),
	Name = "room" ++ RId,
	PName = erlang:list_to_atom(Name),
    gen_fsm:start_link({local, PName}, ?MODULE, [], []).

init([]) ->
    {ok, 'STOP_GAME', #game_state{}}.

join_room(User, RoomId) ->
	Pid = game_room_db:get_game_room_by_roomid(RoomId),
	gen_fsm:send_event(Pid, {join_room, User}).

leave_room(User,RoomId)->
	Pid = game_room_db:get_game_room_by_roomid(RoomId),
	gen_fsm:send_event(Pid, {leave_room, User}).

'WAIT_START'(_Other, State) ->
	{next_state, 'WAIT_START', State}.

'STOP_GAME'({join_room, User}, State) ->
	#game_state{user_list = UserList} = State, 
	NewUserList = UserList ++ [User],
	NewState = State#game_state{user_list = NewUserList},
	
	if length(NewUserList) =:= 3 ->		%%凑够三个人，准备开始游戏
		?DD("ready start game"),
		CallLandlordTime = #game_conf.landlord_time,
		OutCardTime = #game_conf.outcard_time,
		Data = protocol:build_packet(?SERVER_CMD_BC_WAIT_START, <<CallLandlordTime:?SHORT, OutCardTime:?SHORT>>),
		bc_user_packet(NewUserList, Data),

		{next_state, 'WAIT_START', NewState, 2000};
	true ->
		{next_state, 'STOP_GAME', NewState}
	end;
'STOP_GAME'({leave_room, User}, State) ->
	#game_state{user_list = UserList} = State, 
	NewUserList = UserList -- [User],
	NewState = State#game_state{user_list = NewUserList},
	{next_state, 'STOP_GAME', NewState};
'STOP_GAME'(_Other, State) ->
	{next_state, 'STOP_GAME', State}.


cards() ->
[
	16#01,16#02,16#03,16#04,16#05,16#06,16#07,16#08,16#09,16#0A,16#0B,16#0C,16#0D,	%%方块 A - K (14,15,3,4,5,...13)
	16#11,16#12,16#13,16#14,16#15,16#16,16#17,16#18,16#19,16#1A,16#1B,16#1C,16#1D,	%%梅花 A - K (14,15,3,4,5,...13)
	16#21,16#22,16#23,16#24,16#25,16#26,16#27,16#28,16#29,16#2A,16#2B,16#2C,16#2D,	%%红桃 A - K (14,15,3,4,5,...13)
	16#31,16#32,16#33,16#34,16#35,16#36,16#37,16#38,16#39,16#3A,16#3B,16#3C,16#3D,	%%黑桃 A - K (14,15,3,4,5,...13)
	16#4E,16#4F
].

shuffle(Cards) ->
    Temp = lists:map(fun(X) ->
			     {random:uniform(1 bsl 64), X}
		     end,
		     Cards),
    Temp1 = lists:keysort(1, Temp),
    lists:map(fun(X) ->
		      element(2, X)
	      end,
	      Temp1).
  
deal_card(Count) ->
	Cards = shuffle(cards()),
	{Cards1, R1} = lists:split(Count, Cards),	
	{Cards2, R2} = lists:split(Count, R1),	
	{Cards3, BackCard} = lists:split(Count, R2),	
	{Cards1, Cards2, Cards3, BackCard}.

bc_user_packet([], _Packet)->
	ok;
bc_user_packet([User | Rest], Packet) ->
	chatserver:send_packet(User#player.socket, Packet),
	bc_user_packet(Rest, Packet).

%% gen_fsm template 
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(Event, From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


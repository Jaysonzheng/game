-module(card_logic).

-compile(export_all).
-export([shuffle/1, deal_card/1, sort_card/1, get_card_type/1, test/0]).

-include("common.hrl").
-include_lib("eunit/include/eunit.hrl"). 

-define(LOGIC_MASK_COLOR, 16#F0).
-define(LOGIC_MASK_VALUE, 16#0F).

-define(INVALID_CARD_TYPE, 1).

cards() ->
[
	16#01,16#02,16#03,16#04,16#05,16#06,16#07,16#08,16#09,16#0A,16#0B,16#0C,16#0D,	%%方块 A - K (14,15,3,4,5,...13)
	16#11,16#12,16#13,16#14,16#15,16#16,16#17,16#18,16#19,16#1A,16#1B,16#1C,16#1D,	%%梅花 A - K (14,15,3,4,5,...13)
	16#21,16#22,16#23,16#24,16#25,16#26,16#27,16#28,16#29,16#2A,16#2B,16#2C,16#2D,	%%红桃 A - K (14,15,3,4,5,...13)
	16#31,16#32,16#33,16#34,16#35,16#36,16#37,16#38,16#39,16#3A,16#3B,16#3C,16#3D,	%%黑桃 A - K (14,15,3,4,5,...13)
	16#4E,16#4F
].

card_value(Card) ->	Card band ?LOGIC_MASK_VALUE.
card_color(Card) ->	Card band ?LOGIC_MASK_COLOR.

card_logic_value(Card) ->
	Value = card_value(Card), 
	Color = card_color(Card),
	if Color < 0 orelse Color > 16#40 orelse Value < 1 orelse Value > 16#F -> -1;
	true ->	
		if Color =:= 16#40 ->
			Value + 15;
		true ->
			if Value < 3 andalso Value > 0 ->
				Value + 13;
			true ->
				Value
			end
		end
	end.
	
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
	%% ?DEBUG("cards1 = ~p, cards2 = ~p, cards3 = ~p~n", [Cards1, Cards2, Cards3]),
	{Cards1, Cards2, Cards3, BackCard}.

sort_card(Cards) ->
	F = fun(X, Y) ->
			ValX = card_logic_value(X),	 
			ValY = card_logic_value(Y),
			ColX = card_color(X),
			ColY = card_color(Y),
			if ValX =:= ValY -> 
				ColX > ColY;
			true ->	
				ValX > ValY
			end					
	end,
	lists:sort(F, Cards).

get_card_type(Cards) ->
	Len = length(Cards),
	case Len of 
	1 ->
		check_card_type(Cards, [fun is_single/1]);
	2 ->
		check_card_type(Cards, [fun is_missile/1, fun is_double/1]);
	3 ->
		check_card_type(Cards, [fun is_three/1]);
	4 ->
		check_card_type(Cards, [fun is_three_take_single/1, fun is_bomb/1]);
	5 ->
		check_card_type(Cards, [fun is_single_line/1, fun is_three_take_double/1]);
	6 ->
		check_card_type(Cards, [fun is_single_line/1, fun is_double_line/1, fun is_three_line/1, fun is_four_take_single/1]);
	8 ->
		check_card_type(Cards, [fun is_single_line/1, 
								fun is_double_line/1,
								fun is_four_take_double/1, 
								fun is_three_line_take_x/1]);
	_ ->
		check_card_type(Cards, [fun is_single_line/1, 
								fun is_double_line/1, 
								fun is_three_line/1,
								fun is_three_line_take_x/1
								])
	end.

%% check cards type
%%
check_card_type(Cards, []) -> false;
check_card_type(Cards, [Guard|Rest]) ->
	CardType = Guard(Cards), 	
	if CardType == false ->
		check_card_type(Cards, Rest);
	true ->
		CardType
	end.

is_single(Cards) ->	card_logic_value(Cards) /= -1.

is_double(Cards) -> case length(Cards) =:= 2 andalso 
	card_logic_value(lists:nth(1, Cards)) == card_logic_value(lists:nth(2, Cards)) of 
	true ->
		type_double;
	false ->
		false
	end.

is_missile(Cards) ->
	case length(Cards) =:= 2 andalso
		lists:nth(1, Cards) =:= 16#4F andalso 
		lists:nth(2, Cards) =:= 16#4E of
	true -> type_missile;
	false -> false
	end.

is_three(Cards) -> 
	case length(Cards) =:= 3 andalso 
		card_logic_value(lists:nth(1, Cards)) == card_logic_value(lists:nth(2, Cards)) andalso
		card_logic_value(lists:nth(1, Cards)) == card_logic_value(lists:nth(3, Cards)) of 
	true -> type_three;
	false -> false
	end.

is_bomb(Cards) ->
	case length(Cards) =:= 4 andalso 
		card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(2, Cards)) andalso
		card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(3, Cards)) andalso
		card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(4, Cards)) of 
	true -> type_bomb;
	false -> false
	end.

%% 3带1判断
is_three_take_single(Cards) ->
	[Card1, Card2, Card3, Card4] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	case (V1 =:= V2 andalso V1 =:= V3) orelse (V2 =:= V3 andalso V2 =:= V4) of
	true -> type_three_take_single;
	false -> false
	end.


%% 3带2判断
is_three_take_double(Cards) ->
	[Card1, Card2, Card3, Card4, Card5] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	V5 = card_logic_value(Card5),
	case (V1 =:= V2 andalso V1 =:= V3 andalso V4 =:= V5) orelse (V1 =:= V2 andalso V3 =:= V4 andalso V3 =:= V5) of
	true -> type_three_take_double;
	false -> false
	end.

%% 4带2单判断
is_four_take_single(Cards) ->
	[Card1, Card2, Card3, Card4, Card5, Card6] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	V5 = card_logic_value(Card5),
	V6 = card_logic_value(Card6),

	case (V1 =:= V2 andalso V1 =:= V3 andalso V1 =:= V4) orelse
		(V2 =:= V3 andalso V2 =:= V4 andalso V2 =:= V5) orelse
		(V3 =:= V4 andalso V3 =:= V5 andalso V3 =:= V6) of 
	true -> type_four_take_single;
	false -> false
	end.

%% 4带2对判断
is_four_take_double(Cards) ->
	[Card1, Card2, Card3, Card4, Card5, Card6, Card7, Card8] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	V5 = card_logic_value(Card5),
	V6 = card_logic_value(Card6),
	V7 = card_logic_value(Card7),
	V8 = card_logic_value(Card8),

	case (V1 =:= V2 andalso V1 =:= V3 andalso V1 =:= V4 andalso V5 =:= V6 andalso V7 =:= V8) orelse
		(V3 =:= V4 andalso V3 =:= V5 andalso V3 =:= V6 andalso V1 =:= V2 andalso V7 =:= V8) orelse
		(V5 =:= V6 andalso V5 =:= V7 andalso V5 =:= V8 andalso V1 =:= V2 andalso V3 =:= V4) of
	true -> type_four_take_double;
	false -> false
	end.

%% 单连判断
is_single_line(Cards) when length(Cards) < 2 -> false;
is_single_line(Cards) when length(Cards) >= 2 ->
	[E|R] = Cards,
	Value = card_logic_value(E),
	if Value =:= 15 ->	%% 2不能在单联中
		false;
	true ->
		check_line(single_line, -1, Cards)
	end.

%% 连对判断
is_double_line(Cards) when length(Cards) == 0 orelse length(Cards) rem 2 /= 0 -> false;
is_double_line(Cards) when length(Cards) rem 2 =:= 0 ->
	[E|R] = Cards,
	Value = card_logic_value(E),
	if Value =:= 15 ->	%% 2不能在单联中
		false;
	true ->
		check_line(double_line, -1, Cards)
	end.

%% 三连判断
is_three_line(Cards) when length(Cards) < 6 orelse length(Cards) rem 3 /= 0 -> false;
is_three_line(Cards) ->
	[E|R] = Cards,
	Value = card_logic_value(E),
	if length(Cards) rem 3 /= 0 andalso Value /= 15 ->
		false;
	true ->
		check_line(three_line, -1, Cards)
	end.

check_line(CheckType, _Value, []) -> CheckType;
check_line(CheckType, Value, Cards) when CheckType == single_line ->
	[E|R] = Cards,
	Value2 = card_logic_value(E),
	%% ?DEBUG("check_line, val = ~p, val2 = ~p~n", [Value, Value2]),
	if Value =:= Value2 + 1 orelse Value =:= -1 ->
		check_line(CheckType, Value2, R);
	true ->
		false
	end;
check_line(CheckType, Value, Cards) when CheckType =:= double_line ->
	[E1, E2 | R] = Cards,
	V1 = card_logic_value(E1), 
	V2 = card_logic_value(E2),
	if V1 =:= V2 andalso
		(Value =:= V1 + 1 orelse Value =:= -1) ->
		check_line(CheckType, V1, R);
	true ->
		false
	end;
check_line(CheckType, Value, Cards) when CheckType =:= three_line ->
	[E1, E2, E3 | R] = Cards,
	V1 = card_logic_value(E1), 
	V2 = card_logic_value(E2),
	V3 = card_logic_value(E3),
	if V1 =:= V2 andalso
	   V1 =:= V3 andalso
		(Value =:= V1 + 1 orelse Value =:= -1) ->
		check_line(CheckType, V1, R);
	true ->
		false
	end.

count(Key, List) -> count(Key, List, 0).
count(Key, [], Count) -> Count;
count(Key, [E|R], Count) ->
	%{C, _V} = E, 
	?DEBUG("Key = ~p, E = ~p~n", [Key, E]),
	if E == Key ->
		count(Key, R, Count + 1);
	true ->
		count(Key, R, Count)
	end.

check_three_takex_line([], _Value) -> true;
check_three_takex_line([{_Count, Value}|R], PreValue) -> 
	if (Value =:= PreValue - 1 orelse PreValue =:= -1) ->
		check_three_takex_line(R, Value);
	true ->
		false
	end.

%%3带1，3带2连判断
is_three_line_take_x(Cards) -> 
	F = fun(Card) -> card_logic_value(Card) end, 
	L = lists:map(F, Cards),  	
	L2 = lists:usort(L),
	L3 = lists:map(fun(Key) -> {count(Key, L), Key}  end, L2),
	L4 = lists:filter(fun({Count, Key}) -> Count == 3 end, L3), 
	%%L4 = lists:map(fun({Count, Key}) -> if Count == 3 -> Key; true -> false end end, L3), 

	?DEBUG("Cards = ~w, L = ~w, L2 = ~w, L3 =~w, L4 = ~w ~n", [Cards, L, L2, L3, L4]),
	SingleCount = count(1, L3), 
	DoubleCount = count(2, L3), 
	ThreeCount = count(3, L3),
	?DEBUG("1 = ~w, 2 = ~w, 3 = ~w~n", [SingleCount, DoubleCount, ThreeCount]),
	
	Ret = check_three_takex_line(L4, -1),
	if 
		Ret == true andalso SingleCount == ThreeCount -> three_line_take_single;
		Ret == true andalso DoubleCount == ThreeCount -> three_line_take_double;
		true ->	false
	end.
%    F = fun(Card1, Card2) -> 
%            V1 = card_logic_value(Card1),
%            V2 = card_logic_value(Card2),
%            V1 == V2
%        end, 
%    C = lists:usort(F, Cards),
%    C2 = Cards -- C,
%    C3 = lists:reverse(lists:usort(F, C2)),

%    ?DEBUG("Cards = ~w, C = ~w, C2 = ~w, C3 = ~w~n", [Cards, C, C2, C3]),
%    case is_single_line(C3) of 
%    single_line ->
%        ?DD("double line~n"),
%        Count = length(Cards),
%        if Count rem 4 =:= 0 ->
%            three_line_take_single;
%        Count rem 5 =:= 0 ->
%            three_line_take_double;
%        true ->
%            false
%        end;
%    false ->
%        false
%    end.	

%% module test
test() ->
	%?DEBUG("logic val = ~p~n", [card_logic_value(16#32)]),
	%?assert(is_single_line([16#32, 16#21, 16#3D, 16#2C, 16#1B]) == false),
	%?assert(is_single_line([16#31, 16#2D, 16#3C, 16#2B, 16#1A]) == true),
	%?assert(is_single_line([16#31, 16#2D, 16#3C, 16#2B, 16#19]) == false),
	
	%?assert(is_double_line([16#2d, 16#1d, 16#2c, 16#1c, 16#2b, 16#1b]) == true),
	
%    ?assert(is_three_take_single([16#03, 16#34, 16#24, 16#14]) == type_three_take_single),
%    ?assert(is_three_take_single([16#34, 16#24, 16#14, 16#03]) == type_three_take_single),
%    ?assert(is_three_take_single([16#34, 16#24, 16#23, 16#03]) == false),
%    
%    ?assert(is_three_take_double([16#25, 16#15, 16#34, 16#24, 16#14]) == type_three_take_double),
%    ?assert(is_three_take_double([16#25, 16#15, 16#35, 16#24, 16#14]) == type_three_take_double),
%    ?assert(is_three_take_double([16#26, 16#15, 16#35, 16#24, 16#14]) == false),
%    
%    ?assert(get_card_type([16#22, 16#32]) == type_double), 
%    ?assert(get_card_type([16#4F, 16#4E]) == type_missile),
%    ?assert(get_card_type([16#32, 16#22, 16#12]) == type_three),
%    
%    ?assert(get_card_type([16#29, 16#28, 16#37, 16#26, 16#15, 16#14, 16#13]) == single_line),
%    ?assert(get_card_type([16#2A, 16#28, 16#37, 16#26, 16#15, 16#14, 16#13]) /= single_line),
%    ?assert(get_card_type([16#29, 16#28, 16#37, 16#27, 16#15, 16#14, 16#13]) /= single_line),

%    ?assert(get_card_type([16#2d, 16#1d, 16#2c, 16#1c, 16#2b, 16#1b]) == double_line),
%    ?assert(get_card_type([16#2d, 16#1d, 16#2c, 16#1c, 16#2b, 16#1b, 16#1a, 16#0a]) == double_line),
%    ?assert(get_card_type([16#2d, 16#1d, 16#2c, 16#1c]) /= double_line),
%    
%    ?assert(get_card_type([16#2d, 16#1d, 16#0d, 16#2c, 16#1c, 16#0c]) == three_line),
	?assert(get_card_type([16#2d, 16#1d, 16#0d, 16#2d, 16#1d, 16#0d, 16#2c, 16#1c, 16#0c]) /= three_line),
	?assert(get_card_type([16#2d, 16#1d, 16#0d, 16#2c, 16#1c, 16#0c, 16#23, 16#25]) == three_line_take_single),
	?assert(get_card_type([16#2d, 16#1d, 16#0d, 16#2c, 16#1c, 16#0c, 16#25, 16#15, 16#23, 16#13]) == three_line_take_double),

	%Value = is_three_line_take_x([16#2d, 16#1d, 16#0d, 16#2c, 16#1c, 16#0c, 16#23, 16#25]),
	%Value = is_four_line_take_x([16#26, 16#15, 16#35, 16#24, 16#14]),
	%?DEBUG("value = ~p~n", [Value]),
	eunit_test_all_pass.


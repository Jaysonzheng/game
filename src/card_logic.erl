-module(card_logic).

-export([shuffle/1, deal_card/1, sort_card/1, test/0]).

-include("common.hrl").

-define(LOGIC_MASK_COLOR, 16#F0).
-define(LOGIC_MASK_VALUE, 16#0F).

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
	if Color =:= 16#40 ->
		Value + 15;
	true ->
		if Value < 3 andalso Value > 0 ->
			Value + 13;
		true ->
			Value
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


is_bomb(Cards) ->
	length(Cards) =:= 4 andalso 
	card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(2, Cards)) andalso
	card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(3, Cards)) andalso
	card_logic_value(lists:nth(1, Cards)) =:= card_logic_value(lists:nth(4, Cards)).

is_missile(Cards) ->
	length(Cards) =:= 2 andalso
	lists:nth(1, Cards) =:= 16#4F andalso 
	lists:nth(2, Cards) =:= 16#4E.

%% 3带1判断
is_three_take_single(Cards) ->
	[Card1, Card2, Card3, Card4] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	(V1 =:= V2 andalso V1 =:= V3) orelse (V2 =:= V3 andalso V2 =:= V4).

%% 3带2判断
is_three_take_double(Cards) ->
	[Card1, Card2, Card3, Card4, Card5] = Cards, 
	V1 = card_logic_value(Card1),
	V2 = card_logic_value(Card2),
	V3 = card_logic_value(Card3),
	V4 = card_logic_value(Card4),
	V5 = card_logic_value(Card5),
	(V1 =:= V2 andalso V1 =:= V3 andalso V4 =:= V5) orelse (V1 =:= V2 andalso V3 =:= V4 andalso V3 =:= V5).
		
%% 单连判断
is_single_line([E|R]) ->
	Value = card_logic_value(E),
	if Value =:= 2 ->	%% 2不能在单联中
		false;
	true ->
		check_line(single, -1, R)
	end.

%% 连对判断
is_double_line(Cards) when length(Cards) rem 2 /= 0 ->	false;
is_double_line(Cards) when length(Cards) rem 2 =:= 0 ->
	check_line(double, -1, Cards).

check_line(CheckType, Value, []) -> true;
check_line(CheckType, Value, Cards) when CheckType == single ->
	[E|R] = Cards,
	Value2 = card_logic_value(E),
	%% ?DEBUG("check_line, val = ~p, val2 = ~p~n", [Value, Value2]),
	if Value =:= Value2 + 1 orelse Value =:= -1 ->
		check_line(CheckType, Value2, R);
	true ->
		false
	end;
check_line(CheckType, Value, Cards) when CheckType =:= double ->
	[E1, E2 | R] = Cards,
	V1 = card_logic_value(E1), 
	V2 = card_logic_value(E2),
	if V1 =:= V2 andalso
		(Value =:= V1 + 1 orelse Value =:= -1) ->
		check_line(CheckType, V1, R);
	true ->
		false
	end;
check_line(CheckType, Value, Cards) when CheckType =:= three ->
	ok.

is_three_line(Cards) ->
	if length(Cards) rem 3 /= 0 ->
		false;
	true ->
		true
		
	end.


%% module test
test() ->
	C = sort_card(shuffle(cards())),
	V = is_single_line([16#31, 16#2D, 16#3C, 16#2B, 16#19]),
	V2 = is_double_line([16#2d, 16#1d, 16#2c, 16#1c, 16#2a, 16#1a]),
	?DEBUG("after sort, poker :~w~n", [V2]).


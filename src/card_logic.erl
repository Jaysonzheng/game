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




%% module test
test() ->
	C = sort_card(shuffle(cards())),
	?DEBUG("after sort, poker :~w~n", [C]).


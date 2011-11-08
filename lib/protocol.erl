%% Author: xiaoshengaya
%% Created: 2010-1-4
%% Description: 
-module(protocol).

-include("protocol.hrl").

%%
%% Exported Functions
%%
-export([read_string/1,read_int/1, read_uint/1, read_short/1, read_byte/1, read_binary/1, read_int_list/2]).
-export([write_string/2, write_short/2, write_int/2, write_uint/2, write_byte/2, write_binary/3]).
-export([parse_header/1, build_packet/1, build_packet/2]).
-export([write_int_list/2, write_byte_list/2]).
 
%%
%% API Functions
%%

%%--protocol format
%%	0   1   2    3	   5			
%%	+---+---+---+------+----------+
%%	| B | Y |Ver| CMD  |   Body   |
%%	+---+---+---+------+----------+

parse_header(Bin) ->
	<<Version:?BYTE, CmdType:?SHORT, Body/binary>> = Bin,
	%%io:format("Iden1:~w,Iden2:~w, Version:~w~n",[Iden1, Iden2, Version]),
	io:format("Version = ~p, PacketVersion = ~p~n", [Version, ?PACKET_VERSION]),
	if 
		Version /= ?PACKET_VERSION -> {error,version};
		true -> {ok, CmdType, Body}
	end.

%% read data part
read_string(Bin) ->
    <<Len:?DWORD, Bin1/binary>> = Bin,
    <<Str:Len/binary-unit:8, Rest/binary>> = Bin1,
    {binary_to_list(Str), Rest}.
	
read_int(Bin) ->
	<<Value:?LONG, Rest/binary>> = Bin,
	{Value, Rest}.

read_uint(Bin) ->
	<<Value:?DWORD, Rest/binary>> = Bin,
	{Value, Rest}.

read_short(Bin) ->
	<<Value:?SHORT, Rest/binary>> = Bin,
	{Value, Rest}.

read_byte(Bin) ->
	<<Value:?BYTE, Rest/binary>> = Bin,
	{Value, Rest}.

read_binary(Bin) ->
	<<Len:?DWORD, Bin1/binary>> = Bin,
    <<Value:Len/binary-unit:8, Rest/binary>> = Bin1,
	{Value, Rest}.
	
%% write data part
write_string(Bin, Str) ->
	Len = length(Str),
	StrBin = [binary_to_list(<<Len:?DWORD>>) | Str],
	NewBin = [Bin | StrBin],
	list_to_binary(NewBin).

write_short(Bin, Value) ->
    <<Bin/binary, Value:?SHORT>>.

write_int(Bin, Value) ->
	<<Bin/binary, Value:?LONG>>.
	
write_uint(Bin, Value) ->
	<<Bin/binary, Value:?DWORD>>.

write_byte(Bin, Value) ->
	<<Bin/binary, Value:?BYTE>>.

write_binary(Bin, Len, Value) ->
	<<Bin/binary, Len:?DWORD, Value/binary>>.

build_packet(CmdType) ->
	build_packet(CmdType,<<>>).

build_packet(CmdType, Body) ->
	Packet = <<?PACKET_VERSION:?BYTE, CmdType:?SHORT, Body/binary>>,
	%%EncodeBody = encode_data(Body, []),
	%%Packet = list_to_binary([Header | EncodeBody]),
	Packet.
	
read_int_list(<<>>, List) -> List;
read_int_list(<<Elem:?LONG, Bin/binary>>, List) ->
	NewList = [Elem | List],
	read_int_list(Bin, NewList).

write_int_list(Bin, List) ->
	Len = length(List),
	NewBin = <<Bin/binary, Len:?LONG>>,
	write_int_binary(NewBin, List).

write_int_binary(Bin, []) -> Bin;
write_int_binary(Bin, [Elem | List]) ->	
	NewBin = <<Bin/binary, Elem:?LONG>>,
	write_int_binary(NewBin, List).

write_byte_list(Bin, List)->
	Len = length(List),
	NewBin = <<Bin/binary, Len:?LONG>>,
	write_byte_binary(NewBin, List).

write_byte_binary(Bin, []) -> Bin;
write_byte_binary(Bin, [Elem | List]) ->
	NewBin = <<Bin/binary, Elem:?BYTE>>,
	write_byte_binary(NewBin, List).

encode_data(<<>>, List) -> 
	NewList = lists:reverse(List),
	list_to_binary(NewList);
encode_data(<<Elem:?BYTE, Rest/binary>>, List) ->
	Code = encode:encode(Elem),
	NewList = [Code|List],
 	encode_data(Rest, NewList).

decode_data(<<>>, List) -> 
	NewList = lists:reverse(List),
	list_to_binary(NewList);
decode_data(<<Elem:?BYTE, Rest/binary>>, List) ->
	Code = decode:decode(Elem),
	%%NewList = List ++ [Code],		%%这种用法效率低,使用将元素加到列表头,最后调反转列表
	NewList = [Code|List],
 	decode_data(Rest, NewList).

%%
%% Local Functions
%%

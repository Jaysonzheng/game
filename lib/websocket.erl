
-module(websocket).

-export([handshake/1]).

%% draft 76 for safari
handshake(Headers) ->
	[Body|Head] = lists:reverse(string:tokens(Headers, "\r\n")),
	io:format("~n~n, Body = ~p~n~nhead = ~s~n", [Body, Head]),
    {Host, Origin, Key1, Key2} = extract_keys(lists:reverse(Head)),
    {Skey1, Skey2} = process_keys(Key1, Key2),
    Bin_body = list_to_binary(Body),
    Key = <<Skey1:32/big-unsigned-integer, Skey2:32/big-unsigned-integer, Bin_body/binary>>,
	K = erlang:md5(Key),
	{Host, "/", Origin, K}.

% Extract keys from the client's handshake.
extract_keys([H|T])->
    Key1 = extract_key("Sec-WebSocket-Key1: ", [H|T]),
    Key2 = extract_key("Sec-WebSocket-Key2: ", [H|T]),
	Host = extract_key("Host: ", [H|T]),
	Origin = extract_key("Origin: ", [H|T]),
	Path = extract_key("GET ", [H|T]),
	io:format("path = ~p~n", [Path]),
    {Host, Origin, Key1, Key2}.

extract_key(X, [H|T])->
    case string:str(H, X) of
        0 -> extract_key(X, T); 
        _Pos -> string:substr(H, string:len(X) + 1)
    end.

% Process the keys as mentioned in the handshake 76 draft of the ietf.
process_keys(Key1, Key2)->
    {Digits1, []} = string:to_integer(digits(Key1)),
    {Digits2, []} = string:to_integer(digits(Key2)),
    Spaces1 = spaces(Key1),
    Spaces2 = spaces(Key2),
    {Digits1 div Spaces1, Digits2 div Spaces2}.

% Concatenate digits 0-9 of a string 
digits(X)-> [A || A<-X, A =< 57, A >= 48].

% Count number of spaces in a string.
spaces(X)-> string:len([ A || A<-X, A =:= 32]).
	

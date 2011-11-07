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

-record(player, {
			id,				% user Id
			socket,			% socket handle 
            friends,		% friend id list
			status,			% status, 0:offline, 1:online
			roomid = 0,		% room id
			time = none,	% active time
            ip=any          % user ip
			}).

-define (IMSG(X), error_logger:info_msg("~p:~p  ~p~n",[?MODULE, ?LINE, X])). 
-define (IREPORT(X), error_logger:info_report([{module, ?MODULE}, {line, ?LINE}|X])). 
-define (EMSG(X), error_logger:error_msg("~p:~p  ~p~n",[?MODULE, ?LINE, X])). 
-define (EREPORT(X), error_logger:error_report([{module, ?MODULE}, {line, ?LINE}|X])). 
%% Author: xiaoshengaya
%% Created: 2010-1-3
%% Description: define protocol cmd 

-define(PACKET_VERSION, 1).

-define(SERVER_CMD_SYNC, 			16#001).	%%服务端心跳检测包			
-define(CLIENT_CMD_SYNC, 			16#002).	%%客户端心跳回应包

-define(CMD_CLIENT_LOGIN, 			16#101).	%%用户登录
-define(CMD_CLIENT_LOGOUT, 			16#102).	%%用户注销
-define(CMD_CLIENT_CHAT,			16#103).	%%用户聊天	
-define(CMD_CLIENT_BC_ALL_USERS, 	16#104).	%%用户广播所有用户
-define(CMD_CLIENT_BC_FRIENDS,		16#105).	%%用户广播好友		
-define(CMD_CLIENT_ENTER_ROOM,		16#106).	%%用户进入房间	
-define(CMD_CLIENT_LEAVE_ROOM,		16#107).	%%用户退出房间		
-define(CMD_CLIENT_SET_STATUS,		16#108).	%%用户设置在线状态
-define(CMD_CLIENT_GET_USER_COUNT,  16#109).	%%用户获取在线玩家数

-define(SERVER_CMD_LOGINSUCCESS, 	16#201).	%%服务端回应登录成功
-define(SERVER_CMD_LOGINERR, 		16#202).	%%服务端回应登录失败
-define(SERVER_CMD_KICK_OUT,		16#203).	%%服务器踢掉用户
-define(SERVER_CMD_RES_USER_COUNT,  16#204).	%%服务端回应用户数

-define(SERVER_BC_USER_LOGIN,		16#301).	%%服务端广播用户登录
-define(SERVER_BC_USER_LOGOUT,  	16#302).	%%服务端广播

%% 游戏相关的协议
-define(CLIENT_CMD_JOIN_GAME,		16#1001).	%%客户端请求加入游戏

-define(SERVER_CMD_JOIN_GAME_SUCCESS, 16#2001).
-define(SERVER_CMD_JOIN_GAME_FAILED,  16#2002).
-define(SERVER_CMD_BC_WAIT_START,	  16#2003).
-define(SERVER_CMD_DEAL_CARD,		  16#2004).
-define(SERVER_CMD_BC_CALL_LANDLORD,  16#2005).

-define(DWORD, 32/unsigned-little-integer).
-define(LONG,  32/signed-little-integer).
-define(SHORT, 16/signed-little-integer).
-define(BYTE,  8/unsigned-integer).

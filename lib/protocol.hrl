%% Author: xiaoshengaya
%% Created: 2010-1-3
%% Description: define protocol cmd 

-define(PACKET_VERSION, 2).

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
-define(CMD_CLIENT_GET_USER_COUNT,      16#109).        %%用户请求获取在线人数
-define(CMD_CLIENT_GET_USER_IP,     16#110).        %%查询请求获取user的ip

-define(SERVER_CMD_LOGINSUCCESS, 	16#201).	%%服务端回应登录成功
-define(SERVER_CMD_LOGINERR, 		16#202).	%%服务端回应登录失败
-define(SERVER_CMD_KICK_OUT,		16#203).	%%服务器踢掉用户
-define(SERVER_CMD_RES_USER_COUNT,      16#204).        %%服务器回应获取在线用户数

-define(SERVER_CMD_RES_USER_IP,  16#205).        %%服务器回应获取user ip

-define(SERVER_BC_USER_LOGIN,		16#301).	%%服务端广播用户登录
-define(SERVER_BC_USER_LOGOUT,  	16#302).	%%服务端广播

-define(DWORD, 32/unsigned-integer).
-define(LONG,  32/signed-integer).
-define(SHORT, 16/signed-integer).
-define(BYTE,  8/unsigned-integer).

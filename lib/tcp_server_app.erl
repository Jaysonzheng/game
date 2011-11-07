%% Author: xiaoshengaya
%% Created: 2010-2-2
%% Description: 
-module(tcp_server_app).

-behaviour(application).

%% Internal API
-export([start_client/1]).

%% Application and Supervisor callbacks
-export([start/1, start/2, stop/1, init/1]).

-define(MAX_RESTART, 5).
-define(MAX_TIME,    60).
-define(DEF_PORT,    443).
-define(APP_NAME, 	 chatapp).
-define(SERVER_MODULE, server).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client(Module) ->
    supervisor:start_child(tcp_client_sup, []).

start(AppName) ->
	ensure_apps(),
	application:start(AppName).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
	ServerModule = get_app_env(server_module, ?SERVER_MODULE),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, ServerModule]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, ServerModule]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {tcp_listener,start_link,[Port, ServerModule]},% StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [ServerModule]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([ServerModule]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {socket_handler,start_link,[ServerModule]}, % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.

%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    ok.

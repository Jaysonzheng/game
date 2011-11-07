{application, imserver,
 [
  {description, "Pet Chat Server"},
  {vsn, "1.0"},
  {id, "ChatServer"},
  {modules,      [tcp_listener, tcp_socket_handler]},
  {registered,   [tcp_server_sup, tcp_listener]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {tcp_server_app, []}},
  {env, [
             {listen_port, 8888},
             {server_module,  imserver}
         ]}
 ]
}.

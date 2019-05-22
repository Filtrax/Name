-module(pollution_server_sup).
-author("Filip").

%% API
-export([start/0, init/0]).


start() ->
  spawn_link(?MODULE, init, []).

init() ->
  process_flag(trap_exit, true),
  loop().

loop() ->
  pollution_server:start(),
  receive
    {'EXIT', Pid, Reason} -> loop();
    stop -> ok
  end.
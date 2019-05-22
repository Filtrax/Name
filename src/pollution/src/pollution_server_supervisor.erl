-module(pollution_server_supervisor).
-author("Filip").

-behaviour(supervisor).

%% API
-export([start_link/1, init/1]).
-export([start/0]).

start() -> start_link(empty).

start_link(_InitValue) ->
  supervisor:start_link(
    {local, server_supervisor},
    ?MODULE,
    []
  ).

init(_) ->
  {ok, {
    {one_for_all, 2, 3},
    [{pollution_gen_server,
      {pollution_gen_server, start, []},
      permanent,
      brutal_kill,
      worker,
      [pollution_gen_server]}
    ]}
  }.
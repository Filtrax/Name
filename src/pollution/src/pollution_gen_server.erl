-module(pollution_gen_server).
-behaviour(gen_server).
-author("Filip").

%% API
-export([init/1, start_link/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getHourlyMean/3]).
-export([crash/0]).

start_link(_InitialValue) ->
  gen_server:start_link(
    {local, pollution_gen_server},
    pollution_gen_server,
    empty, []
  ).

init(_) ->
  Monitor = pollution:createMonitor(),
  {ok, Monitor}.

start() -> start_link(empty).
stop() -> gen_server:call(pollution_gen_server, terminate).

terminate(normal, N) -> ok.

crash() -> gen_server:cast(pollution_gen_server, crash).

addStation(Name, Coordinates) -> gen_server:call(pollution_gen_server, {addStation, {Name, Coordinates}}).

addValue(StationKey, Date, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, {StationKey, Date, Type, Value}}).

removeValue(StationKey, Date, Type) -> gen_server:call(pollution_gen_server, {removeValue, {StationKey, Date, Type}}).

getOneValue(StationKey, Date, Type) -> gen_server:call(pollution_gen_server, {getOneValue, {StationKey, Date, Type}}).

getStationMean(StationKey, Type) -> gen_server:call(pollution_gen_server, {getStationMean, {StationKey, Type}}).

getDailyMean(DayOnly, Type) -> gen_server:call(pollution_gen_server, {getDailyMean, {DayOnly, Type}}).

getHourlyMean(StationKey, TimeOnly, Type) ->
  gen_server:call(pollution_gen_server, {getHourlyMean, {StationKey, TimeOnly, Type}}).

handle_call({addStation, {Name, Coordinates}}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Name, Coordinates, Monitor),
  {reply, {Name, Coordinates}, NewMonitor};

handle_call({addValue, {StationKey, Date, Type, Value}}, _From, Monitor) ->
  NewMonitor = pollution:addValue(StationKey, Date, Type, Value, Monitor),
  {reply, {StationKey, Date, Type, Value}, NewMonitor};

handle_call({removeValue, {StationKey, Date, Type}}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(StationKey, Date, Type, Monitor),
  {reply, {StationKey, Date, Type}, NewMonitor};

handle_call({getOneValue, {StationKey, Date, Type}}, _From, Monitor) ->
  Result = pollution:getOneValue(StationKey, Date, Type, Monitor),
  {reply, Result, Monitor};

handle_call({getStationMean, {StationKey, Type}}, _From, Monitor) ->
  Result = pollution:getStationMean(StationKey, Type, Monitor),
  {reply, Result, Monitor};

handle_call({getDailyMean, {DayOnly, Type}}, _From, Monitor) ->
  Result = pollution:getDailyMean(DayOnly, Type, Monitor),
  {reply, Result, Monitor};

handle_call(terminate, _From, Monitor) -> {stop, normal, ok, Monitor}.

handle_cast(crash, Monitor) ->
  Illegal = 1 / 0,
  {noreply, Monitor}.
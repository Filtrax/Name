-module(pollution_server).
-author("Filip").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getMaximumGrowthTime/3, getMaximumGrowthTime/2, crash/0, init/0]).

start() ->
  register(server, spawn_link(?MODULE, init, [])).

stop() ->
  server ! stop.

init() ->
  Monitor = pollution:createMonitor(),
  loop(Monitor).

crash() -> server ! crash.

loop(Monitor) ->
  receive
    {addStation, {Name, Coordinates}} ->
      NewMonitor = pollution:addStation(Name, Coordinates, Monitor),
      loop(NewMonitor);
    {addValue, {StationKey, Date, Type, Value}} ->
      NewMonitor = pollution:addValue(StationKey, Date, Type, Value, Monitor),
      loop(NewMonitor);
    {removeValue, {StationKey, Date, Type}} ->
      NewMonitor = pollution:removeValue(StationKey, Date, Type, Monitor),
      loop(NewMonitor);
    {getOneValue, {StationKey, Date, Type}} ->
      Result = pollution:getOneValue(StationKey, Date, Type, Monitor),
      io:format("Returned value: ~w~n", [Result]),
      loop(Monitor);
    {getStationMean, {StationKey, Type}} ->
      Result = pollution:getStationMean(StationKey, Type, Monitor),
      io:format("Returned value: ~w~n", [Result]),
      loop(Monitor);
    {getDailyMean, {DayOnly, Type}} ->
      Result = pollution:getDailyMean(DayOnly, Type, Monitor),
      io:format("Returned value: ~w~n", [Result]),
      loop(Monitor);
    {getMaximumGrowthTime, {Type, StationKey, Day}} ->
      Result = pollution:getMaximumGrowthTime(Type, StationKey, Day, Monitor),
      io:format("Returned value: ~w~n", [Result]),
      loop(Monitor);
    {getMaximumGrowthTime, {Type, StationKey}} ->
      Result = pollution:getMaximumGrowthTime(Type, StationKey, Monitor),
      io:format("Returned value: ~w~n", [Result]),
      loop(Monitor);
    crash -> 1 / 0;
    stop -> ok
  end.

addStation(Name, Coordinates) -> server ! {addStation, {Name, Coordinates}}.

addValue(StationKey, Date, Type, Value) -> server ! {addValue, {StationKey, Date, Type, Value}}.

removeValue(StationKey, Date, Type) -> server ! {removeValue, {StationKey, Date, Type}}.

getOneValue(StationKey, Date, Type) -> server ! {getOneValue, {StationKey, Date, Type}}.

getStationMean(StationKey, Type) -> server ! {getStationMean, {StationKey, Type}}.

getDailyMean(DayOnly, Type) -> server ! {getDailyMean, {DayOnly, Type}}.

getMaximumGrowthTime(Type, StationKey, Day) -> server ! {getMaximumGrowthTime, {Type, StationKey, Day}}.

getMaximumGrowthTime(Type, StationKey) -> server ! {getMaximumGrowthTime, {Type, StationKey}}.

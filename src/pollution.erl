-module(pollution).
-author("Filip").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/3, getMaximumGrowthTime/4, getMaximumGrowthTime/3]).

-record(measurement, {type, date, value}).
-record(station, {id, name, location, measurements}).
-record(monitor, {names, locations, stations, counter = 0}).

%% create empty monitor
createMonitor() -> #monitor { names = #{}, locations = #{}, stations = #{} }.

%% add station to monitor
addStation(Name, Coordinates, Monitors) ->
  case {maps:is_key(Name, Monitors#monitor.names), maps:is_key(Coordinates, Monitors#monitor.locations)} of
    {false, false} ->
      Id = Monitors#monitor.counter,
      NewStation = #station{
        id = Id,
        name = Name,
        location = Coordinates,
        measurements = []
      },
      Monitors#monitor{
        names = maps:put(Name, Id, Monitors#monitor.names),
        locations = maps:put(Coordinates, Id, Monitors#monitor.locations),
        stations = maps:put(Id, NewStation, Monitors#monitor.stations),
        counter = Id + 1
      };
    {_, _} -> Monitors
  end.

%% add measurement
addValue(StationKey, Date, Type, Value, Monitors) ->
  StationToUpdate = getStation(StationKey, Monitors),
  StationID = getStationID(StationKey, Monitors),
  TypeAtom = getType(Type),
  case getMeasurement(StationToUpdate, Date, TypeAtom) of
    [] ->
      NewMeasurement = #measurement{
        type = TypeAtom,
        date = Date,
        value = Value
      },
      OldMeasurements = StationToUpdate#station.measurements,
      UpdatedStation = StationToUpdate#station{
        measurements = [NewMeasurement] ++ OldMeasurements
      },
      Monitors#monitor{
        stations = maps:put(StationID, UpdatedStation, Monitors#monitor.stations)
      };
    _ -> Monitors
  end.

%% remove measurement
removeValue(StationKey, Date, Type, Monitors) ->
  StationToUpdate = getStation(StationKey, Monitors),
  StationID = getStationID(StationKey, Monitors),
  TypeAtom = getType(Type),
  MeasurementToRemove = getMeasurement(StationToUpdate, Date, TypeAtom),
  OldMeasurements = StationToUpdate#station.measurements,
  UpdatedStation = StationToUpdate#station{
    measurements = OldMeasurements -- MeasurementToRemove
  },
  Monitors#monitor{
    stations = maps:put(StationID, UpdatedStation, Monitors#monitor.stations)
  }.

%% get one value from station of given type
getOneValue(StationKey, Date, Type, Monitors) ->
  Station = getStation(StationKey, Monitors),
  TypeAtom = getType(Type),
  [Measurement] = getMeasurement(Station, Date, TypeAtom),
  Measurement#measurement.value.

%% get station mean value of given type
getStationMean(StationKey, Type, Monitors) ->
  Station = getStation(StationKey, Monitors),
  TypeAtom = getType(Type),
  Values = getValues(Station, TypeAtom),
  Sum = lists:foldl(fun(X, Y) -> X + Y end, 0, Values),
  Length = length(Values),
  Sum / Length.

%% get daily mean form all stations
getDailyMean(DayOnly, Type, Monitors) ->
  Values = getDayValues(DayOnly, getType(Type), Monitors),
  Sum = lists:foldl(fun(X, Y) -> X + Y end, 0, Values),
  Length = length(Values),
  Sum / Length.

getMaximumGrowthTime(Type, StationKey, Day, Monitors) ->
  Station = getStation(StationKey,Monitors),
  Measurements = getDayStationMeasurements(Day, getType(Type), Station),
  Sort = fun(M1, M2) -> isEarlier(getTime(M1#measurement.date), getTime(M2#measurement.date)) end,
  SortedMeasurements = lists:sort(Sort , Measurements),
  check(SortedMeasurements, 0, {0,0,0}).

getMaximumGrowthTime(Type, StationKey, Monitors) ->
  Station = getStation(StationKey,Monitors),
  Measurements = getStationMeasurements(getType(Type), Station),
  Sort = fun(M1, M2) -> isTimeEarlier(getTime(M1#measurement.date), getTime(M2#measurement.date)) end,
  SortedMeasurements = lists:sort(Sort , Measurements),
  check(SortedMeasurements, 0, {0,0,0}).

%% utility "private" functions---------------------------------------------------------------------

check([],_,T) -> T;
check([_],_,T) -> T;
check([X,Y|Z],A,T) ->
  if
    Y#measurement.value - X#measurement.value >= A -> check([Y|Z],Y#measurement.value - X#measurement.value,getTime(y#measurement.date));
    true -> check([Y|Z], A, T)
  end.

getStationID(KeyToFind, Monitors) ->
  case KeyToFind of
    {_, _} -> maps:get(KeyToFind, Monitors#monitor.locations);
    _ -> maps:get(KeyToFind, Monitors#monitor.names)
  end.

getStation(KeyToFind, Monitors) -> maps:get(getStationID(KeyToFind, Monitors), Monitors#monitor.stations).

isAtThisTimeAndType(Measurement, Date, TypeAtom) ->
  (Measurement#measurement.date =:= Date) and (Measurement#measurement.type =:= TypeAtom).

getMeasurement(Station, Date, TypeAtom) ->
  [X || X <- Station#station.measurements, isAtThisTimeAndType(X, Date, TypeAtom)].

getValues(Station, TypeAtom) ->
  [X#measurement.value || X <- Station#station.measurements, X#measurement.type =:= TypeAtom].

getDayValues(DayOnly, TypeAtom, Monitor) ->
  [X#measurement.value || Stations <- getStations(Monitor), X <- Stations#station.measurements,
                        (DayOnly =:= getDay(X#measurement.date)) and (X#measurement.type =:= TypeAtom)].

getDayStationMeasurements(DayOnly, TypeAtom, Station) ->
  [X || X <- Station#station.measurements, (DayOnly =:= getDay(X#measurement.date)) and (X#measurement.type =:= TypeAtom)].

getStationMeasurements(TypeAtom, Station) ->
  [X || X <- Station#station.measurements, X#measurement.type =:= TypeAtom].

getTimeValues(TimeOnly, TypeAtom, Station) ->
  [X#measurement.value || X <- Station#station.measurements, (TimeOnly =:= getTime(X#measurement.date)) and (X#measurement.type =:= TypeAtom)].

getDay(Date) ->
  {Day, _} = Date,
  Day.

getTime(Date) ->
  {_, Time} = Date,
  Time.

isTimeEarlier(Time1, Time2) ->
  {Hour1, Minute1, Second1} = Time1,
  {Hour2, Minute2, Second2} = Time2,
  case {Hour1 < Hour2, Hour1 = Hour2}of
    {false,false} -> false;
    {true,false} -> true;
    {_,_} -> case {Minute1 < Minute2, Minute1 = Minute2} of
               {false,false} -> false;
               {true,false} -> true;
               _ -> Second1 =< Second2
         end
  end.

isEarlier(Date1,Date2) ->
  {{Year1, Month1, Day1},Time1} = Date1,
  {{Year2, Month2, Day2},Time2} = Date2,
  case {Year1 < Year2, Year1 = Year2}of
    {false,false} -> false;
    {true,false} -> true;
    {_,_} -> case {Month1 < Month2, Month1 = Month2} of
               {false,false} -> false;
               {true,false} -> true;
               _ -> case {Day1 < Day2, Day1 = Day2} of
                      {false,false} -> false;
                      {true,false} -> true;
                      _ -> isTimeEarlier(Time1,Time2)
                    end
             end
  end.

getStations(Monitor) ->
  [maps:get(X, Monitor#monitor.stations) || X <- lists:seq(0, Monitor#monitor.counter - 1)].

getType(Type) ->
  case Type of
    "PM10" -> pm10;
    "PM2,5" -> pm25;
    "TEMP" -> temp;
    _ -> other
  end.
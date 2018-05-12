%%%-------------------------------------------------------------------
%%% @author Mateusz Sokol
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. maj 201SS8 17:04
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Mateusz Sokol").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([main/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getMinimumPollutionStation/3,
  start/0, stop/0, loop/1, init/0]).

-record(monitor, {stationMap}).
-record(station, {name, coords, measureList}).
-record(measurement, {type, val, date}).

loop(Monitor) ->
  receive
    "stop" ->
      ok;
    {addStation, N, C} ->
      loop(addStation(Monitor, N, C));
    {addValue, StationName, Date, Type, Value} ->
      loop(addValue(Monitor, StationName, Date, Type, Value));
    {removeValue, StationName, Date, Type} ->
      loop(removeValue(Monitor, StationName, Date, Type));
    {getOneValue, Type, StationName, Date} ->
      io:write(getOneValue(Monitor, Type, StationName, Date)),
      loop(Monitor);
    {getStationMean, Type, StationName} ->
      io:write(getStationMean(Monitor, Type, StationName)),
      loop(Monitor);
    {getDailyMean, Type, Date} ->
      io:write(getDailyMean(Monitor, Type, Date)),
      loop(Monitor);
    {getMinimumPollutionStation, Type, Date} ->
      io:write(getMinimumPollutionStation(Monitor, Type, Date)),
      loop(Monitor);
    {show} ->
      io:write(Monitor),
      loop(Monitor)
  after
    20000 -> ok
  end.

init() ->
  loop(createMonitor()).

start() ->
  Pid = spawn(?MODULE, init, []),
  register(monitor, Pid).

stop() ->
  monitor ! "stop".

main() ->
  start(),
  monitor ! {addStation, "Stat1", {10, 12}},
  monitor ! {addValue, "Stat1", {2018,4,23}, 1, 10},
  monitor ! {addValue, "Stat1", {2018,4,24}, 1, 12},
  monitor ! {addValue, "Stat1", {2018,4,25}, 1, 9},
  monitor ! {removeValue, "Stat1", {2018,4,25}, 1},
  monitor ! {addStation, "Stat2", {20.0, 25.0}},
  monitor ! {addValue, "Stat2", {2018,4,23}, 1, 15},
  monitor ! {getMinimumPollutionStation, 1, {2018,4,23}},
  stop().


createMonitor() -> #monitor{stationMap = #{}}.

%% dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
addStation(#monitor{stationMap = SM}, Name, Coords) ->
  #monitor{stationMap = SM#{Name => #station{name = Name, coords = Coords, measureList = []}}}.

%% dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
addValue(#monitor{stationMap = SM}, StationName, Date, Type, Value) ->
  M = #measurement{type = Type, date = Date, val = Value},
  #station{name = N, coords = C, measureList = ML} = maps:get(StationName, SM),
  Bool = lists:member(M, ML),
  if Bool ->
    #monitor{stationMap = SM};
    true ->
      S = #station{name = N, coords = C, measureList = [M | ML]},
      #monitor{stationMap = SM#{StationName => S}} end.

%% usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
removeValue(#monitor{stationMap = SM}, StationName, Date, Type) ->
  #station{coords = C, measureList = ML} = maps:get(StationName, SM),
  L1 = lists:filter(fun(#measurement{type = T, date = D}) -> (T /= Type) or (D /= Date) end, ML),
  #monitor{stationMap = SM#{StationName => #station{name = StationName, coords = C, measureList = L1}}}.

%% zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
getOneValue(#monitor{stationMap = SM}, Type, StationName, Date) ->
  #station{measureList = ML} = maps:get(StationName, SM),
  [H | _] = lists:filter(fun(M) when
    (M#measurement.type == Type) and
      (M#measurement.date == Date) -> true; (_) -> false end, ML),
  H#measurement.val.

%% zwraca średnią wartość parametru danego typu z zadanej stacji;
getStationMean(#monitor{stationMap = SM}, Type, StationName) ->
  #station{measureList = ML} = maps:get(StationName, SM),
  L = lists:filter(fun(M) when M#measurement.type == Type -> true; (_) -> false end, ML),
  Len = length(L),
  LSum = lists:foldl(fun(M, Sum) -> M#measurement.val + Sum end, 0, L),
  LSum / Len.

%% zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;
getDailyMean(#monitor{stationMap = SM}, Type, Date) ->
  SL = maps:values(SM),
  {L, Len} = getValsAndLength(SL, Type, Date),
  if Len == 0 ->
    0;
    true ->
      L / Len
  end.

getValsAndLength([], _Type, _Date) ->
  {0, 0};
getValsAndLength([#station{name = _Name, coords = _Coords, measureList = MeasureList} | T], Type, Date) ->
  {Sum, Len} = computeMeasureList(MeasureList, Type, Date),
  {Sum1, Len1} = getValsAndLength(T, Type, Date),
  {Sum + Sum1, Len + Len1}.

computeMeasureList(ML, Type, Date) ->
  R = findVals(ML, Type, Date),
  {lists:sum(R), length(R)}.

findVals([], _Type, _Date) ->
  [];
findVals([#measurement{type = Type, val = V, date = Date} | T], Type, Date) ->
  [V | findVals(T, Type, Date)];
findVals([#measurement{type = _,
  val = _V, date = _Date} | T], Type, Date) ->
  findVals(T, Type, Date).

%%  wyszuka stacje z najniższym zanieczyszczeniem danego typu
getMinimumPollutionStation(#monitor{stationMap = SM}, Type, Date) ->
  SL = maps:values(SM),
  findMinInAll(SL, Type, Date, {100000, ""}).

findMinInAll([], _, _, {Min, Stat}) ->
  {Min, Stat};
findMinInAll([#station{name = N, measureList = ML} | T], Type, Date, {Min, Stat}) ->
  V = findMinInStation(ML, Type, Date, 1000000),
  if (V < Min) ->
    findMinInAll(T, Type, Date, {V, N});
    true ->
      findMinInAll(T, Type, Date, {Min, Stat})
  end.

findMinInStation([], _, _, Min) -> Min;
findMinInStation([#measurement{type = Type, val = V, date = Date} | T], Type, Date, Min) when V < Min ->
  findMinInStation(T, Type, Date, V);
findMinInStation([_ | T], Type, Date, Min) -> findMinInStation(T, Type, Date, Min).

%% ----------- TESTS -----------

createMonitor_test() -> #monitor{stationMap = #{}} = createMonitor().

getOneValue_test() ->
  M = createMonitor(),
  M1 = addStation(M, "Stat1", {10, 12}),
  M2 = addValue(M1, "Stat1", {2018,4,23}, 1, 10),
  M3 = addValue(M2, "Stat1", {2018,4,24}, 1, 12),
  M4 = addValue(M3, "Stat1", {2018,4,25}, 1, 9),
  M5 = removeValue(M4, "Stat1", {2018,4,24}, 1),
  ?assert(getOneValue(M5, 1, "Stat1", {2018,4,23}) =:= 10).

getStationMean_test() ->
  M = createMonitor(),
  M1 = addStation(M, "Stat1", {10, 12}),
  M2 = addValue(M1, "Stat1", {2018,4,23}, 1, 10),
  M3 = addValue(M2, "Stat1", {2018,4,24}, 1, 12),
  M4 = addValue(M3, "Stat1", {2018,4,25}, 1, 9),
  M5 = removeValue(M4, "Stat1", {2018,4,25}, 1),
  ?assert(getStationMean(M5, 1, "Stat1") =:= 11.0).

getDailyMean_test() -> M = createMonitor(),
  M1 = addStation(M, "Stat1", {10, 12}),
  M2 = addValue(M1, "Stat1", {2018,4,23}, 1, 10),
  M3 = addValue(M2, "Stat1", {2018,4,24}, 1, 12),
  M4 = addValue(M3, "Stat1", {2018,4,25}, 1, 9),
  M5 = removeValue(M4, "Stat1", {2018,4,24}, 1),
  M6 = addStation(M5, "Stat2", {20.0, 25.0}),
  M7 = addValue(M6, "Stat2", {2018,4,23}, 1, 6),
  ?assert(getDailyMean(M7, 1, {2018,4,23}) =:= 8.0).

getMinimumPollutionStation_test() ->
  M = createMonitor(),
  M1 = addStation(M, "Stat1", {10, 12}),
  M2 = addValue(M1, "Stat1", {2018,4,23}, 1, 10),
  M3 = addValue(M2, "Stat1", {2018,4,24}, 1, 12),
  M4 = addValue(M3, "Stat1", {2018,4,25}, 1, 9),
  M5 = removeValue(M4, "Stat1", {2018,4,25}, 1),
  M6 = addStation(M5, "Stat2", {20.0, 25.0}),
  M7 = addValue(M6, "Stat2", {2018,4,23}, 1, 5),
  ?assert(getMinimumPollutionStation(M7, 1, {2018,4,23}) =:= {5,[83,116,97,116,50]}).

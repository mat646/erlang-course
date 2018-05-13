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
  start/0, stop/1, loop/1, init/0]).

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
    {getOneValue, Type, StationName, Date, Pid} ->
      Pid ! (getOneValue(Monitor, Type, StationName, Date)),
      loop(Monitor);
    {getStationMean, Type, StationName, Pid} ->
      Pid ! (getStationMean(Monitor, Type, StationName)),
      loop(Monitor);
    {getDailyMean, Type, Date, Pid} ->
      Pid ! (getDailyMean(Monitor, Type, Date)),
      loop(Monitor);
    {getMinimumPollutionStation, Type, Date, Pid} ->
      Pid ! (getMinimumPollutionStation(Monitor, Type, Date)),
      loop(Monitor);
    {show, Pid} ->
      Pid ! Monitor,
      loop(Monitor)
  after
    20000 -> ok
  end.

init() ->
  loop(createMonitor()).

start() ->
  spawn(?MODULE, init, []).

stop(Pid) ->
  Pid ! "stop".

main() ->
  Pid = start(),
  Pid ! {addStation, "Stat1", {10, 12}},
  Pid ! {addValue, "Stat1", {2018, 4, 23}, 1, 10},
  Pid ! {addValue, "Stat1", {2018, 4, 24}, 1, 12},
  Pid ! {addValue, "Stat1", {2018, 4, 25}, 1, 9},
  Pid ! {removeValue, "Stat1", {2018, 4, 25}, 1},
  Pid ! {addStation, "Stat2", {20.0, 25.0}},
  Pid ! {addValue, "Stat2", {2018, 4, 23}, 1, 15},
  Pid ! {getMinimumPollutionStation, 1, {2018, 4, 23}, self()},
  receive
    Reply -> io:write(Reply)
  end,
  stop(Pid).


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

createMonitor_test() ->
  Pid = start(),
  Pid ! {show, self()},
  receive
    Reply -> ?assert(Reply =:= #monitor{stationMap = #{}})
  end.

getOneValue_test() ->
  Pid = start(),
  Pid ! {addStation, "Stat1", {10, 12}},
  Pid ! {addValue, "Stat1", {2018, 4, 23}, 1, 10},
  Pid ! {addValue, "Stat1", {2018, 4, 24}, 1, 12},
  Pid ! {addValue, "Stat1", {2018, 4, 25}, 1, 9},
  Pid ! {removeValue, "Stat1", {2018, 4, 24}, 1},
  Pid ! {getOneValue, 1, "Stat1", {2018, 4, 23}, self()},
  receive
    Reply -> ?assert(Reply =:= 10)
  end.

getStationMean_test() ->
  Pid = start(),
  Pid ! {addStation, "Stat1", {10, 12}},
  Pid ! {addValue, "Stat1", {2018, 4, 23}, 1, 10},
  Pid ! {addValue, "Stat1", {2018, 4, 24}, 1, 12},
  Pid ! {addValue, "Stat1", {2018, 4, 25}, 1, 9},
  Pid ! {removeValue, "Stat1", {2018, 4, 25}, 1},
  Pid ! {getStationMean, 1, "Stat1", self()},
  receive
    Reply -> ?assert(Reply =:= 11.0)
  end.

getDailyMean_test() ->
  Pid = start(),
  Pid ! {addStation, "Stat1", {10, 12}},
  Pid ! {addValue, "Stat1", {2018, 4, 23}, 1, 10},
  Pid ! {addValue, "Stat1", {2018, 4, 24}, 1, 12},
  Pid ! {addValue, "Stat1", {2018, 4, 25}, 1, 9},
  Pid ! {removeValue, "Stat1", {2018, 4, 24}, 1},
  Pid ! {addStation, "Stat2", {20.0, 25.0}},
  Pid ! {addValue, "Stat2", {2018, 4, 23}, 1, 6},
  Pid ! {getDailyMean, 1, {2018, 4, 23}, self()},
  receive
    Reply -> ?assert(Reply =:= 8.0)
  end.

getMinimumPollutionStation_test() ->
  Pid = start(),
  Pid ! {addStation, "Stat1", {10, 12}},
  Pid ! {addValue, "Stat1", {2018, 4, 23}, 1, 10},
  Pid ! {addValue, "Stat1", {2018, 4, 24}, 1, 12},
  Pid ! {addValue, "Stat1", {2018, 4, 25}, 1, 9},
  Pid ! {removeValue, "Stat1", {2018, 4, 25}, 1},
  Pid ! {addStation, "Stat2", {20.0, 25.0}},
  Pid ! {addValue, "Stat2", {2018, 4, 23}, 1, 15},
  Pid ! {getMinimumPollutionStation, 1, {2018, 4, 23}, self()},
  receive
    Reply -> ?assert(Reply =:= {10, [83, 116, 97, 116, 49]})
  end.

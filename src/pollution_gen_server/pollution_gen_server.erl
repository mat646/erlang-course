%%%-------------------------------------------------------------------
%%% @author mateusz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2018 13:59
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Mateusz Sokol").
-behaviour(gen_server).

-export([start_link/0, show/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getMinimumPollutionStation/2, crush/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(monitor, {stationMap}).
-record(station, {name, coords, measureList}).
-record(measurement, {type, val, date}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #monitor{stationMap = #{}}}.

handle_call(show, _From, State) ->
  {reply, State, State};

handle_call({add_station, Name, Coords}, _From, #monitor{stationMap = SM}) ->
  {reply, added, #monitor{stationMap = SM#{Name => #station{name = Name, coords = Coords, measureList = []}}}};

handle_call({add_value, StationName, Date, Type, Value}, _From, #monitor{stationMap = SM}) ->
  M = #measurement{type = Type, date = Date, val = Value},
  #station{name = N, coords = C, measureList = ML} = maps:get(StationName, SM),
  Bool = lists:member(M, ML),
  if Bool ->
    {reply, ignored, #monitor{stationMap = SM}};
    true ->
      S = #station{name = N, coords = C, measureList = [M | ML]},
      {reply, added, #monitor{stationMap = SM#{StationName => S}}} end;

handle_call({remove_value, StationName, Date, Type}, _From, #monitor{stationMap = SM}) ->
  #station{coords = C, measureList = ML} = maps:get(StationName, SM),
  L1 = lists:filter(fun(#measurement{type = T, date = D}) -> (T /= Type) or (D /= Date) end, ML),
  {reply, removed, #monitor{stationMap = SM#{StationName => #station{name = StationName, coords = C, measureList = L1}}}};

handle_call({get_one_value, Type, StationName, Date}, _From, #monitor{stationMap = SM}) ->
  #station{measureList = ML} = maps:get(StationName, SM),
  [H | _] = lists:filter(fun(M) when
    (M#measurement.type == Type) and
      (M#measurement.date == Date) -> true; (_) -> false end, ML),
  {reply, H#measurement.val, #monitor{stationMap = SM}};

handle_call({get_station_mean, Type, StationName}, _From, #monitor{stationMap = SM}) ->
  #station{measureList = ML} = maps:get(StationName, SM),
  L = lists:filter(fun(M) when M#measurement.type == Type -> true; (_) -> false end, ML),
  Len = length(L),
  LSum = lists:foldl(fun(M, Sum) -> M#measurement.val + Sum end, 0, L),
  {reply, LSum / Len, #monitor{stationMap = SM}};

handle_call({get_daily_mean, Type, Date}, _From, #monitor{stationMap = SM}) ->
  SL = maps:values(SM),
  {L, Len} = getValsAndLength(SL, Type, Date),
  if Len == 0 ->
    {reply, 0, #monitor{stationMap = SM}};
    true ->
      {reply, L / Len, #monitor{stationMap = SM}}
  end;

handle_call({get_minimum_pollution_station, Type, Date}, _From, #monitor{stationMap = SM}) ->
  SL = maps:values(SM),
  {reply, findMinInAll(SL, Type, Date, {100000, ""}), #monitor{stationMap = SM}};

handle_call(_Request, _From, State) ->
  io:write("Not recognized~n"),
  {reply, ignored, State}.

handle_cast(crash, State) ->
  1/0,
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Pollution server functions

crush() ->
  gen_server:cast(?MODULE, crash).

show() ->
  gen_server:call(
    ?MODULE,
    show).

%% dodaje do monitora wpis o nowej stacji pomiarowej (nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor;
addStation(Name, Coords) ->
  gen_server:call(
    ?MODULE,
    {add_station, Name, Coords}
  ).

%% dodaje odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru, wartość), zwraca zaktualizowany monitor;
addValue(StationName, Date, Type, Value) ->
  gen_server:call(
    ?MODULE,
    {add_value, StationName, Date, Type, Value}
  ).

%% usuwa odczyt ze stacji (współrzędne geograficzne lub nazwa stacji, data, typ pomiaru), zwraca zaktualizowany monitor;
removeValue(StationName, Date, Type) ->
  gen_server:call(
    ?MODULE,
    {remove_value, StationName, Date, Type}
  ).

%% zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
getOneValue(Type, StationName, Date) ->
  gen_server:call(
    ?MODULE,
    {get_one_value, Type, StationName, Date}
  ).

%% zwraca średnią wartość parametru danego typu z zadanej stacji;
getStationMean(Type, StationName) ->
  gen_server:call(
    ?MODULE,
    {get_station_mean, Type, StationName}
  ).

%% zwraca średnią wartość parametru danego typu, danego dnia na wszystkich stacjach;
getDailyMean(Type, Date) ->
  gen_server:call(
    ?MODULE,
    {get_daily_mean, Type, Date}
  ).

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
getMinimumPollutionStation(Type, Date) ->
  gen_server:call(
    ?MODULE,
    {get_minimum_pollution_station, Type, Date}
  ).

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

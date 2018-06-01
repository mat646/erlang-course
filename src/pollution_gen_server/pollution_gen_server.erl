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

-export([start_link/0, say_hello/0, addStation/2]).

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

say_hello() ->
  gen_server:call(
    ?MODULE,
    say_hello).

addStation(Name, Coords) ->
  gen_server:call(
    ?MODULE,
    {add_station, Name, Coords}
  ).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #monitor{stationMap = #{}}}.

handle_call(show, _From, State) ->
  io:write(State),
  {reply, ignored, State};
handle_call({add_station, Name, Coords}, _From, #monitor{stationMap = SM}) ->
  {reply, ignored, #monitor{stationMap = SM#{Name => #station{name = Name, coords = Coords, measureList = []}}}};
handle_call(_Request, _From, State) ->
  io:write(State),
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Pollution server functions





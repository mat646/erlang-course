%%%-------------------------------------------------------------------
%%% @author Mateusz Sokol
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2018 13:05
%%%-------------------------------------------------------------------
-module(qsort).
-author("Mateusz Sokol").

%% API
-export([main/0, qs/1]).

main() -> io:fwrite(compareSpeeds([1, 2, 3, 4], fun qsort:qs/1, fun lists:sort/1)).

%% qs(randomElems(5, 1, 8))

%% qs([5, 7, 1, 14, 8])

lessThan(List, Arg) -> [X || X <- List, X < Arg].

grtEqThan(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(lessThan(Tail, Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail, Pivot)).

randomElems(N, Min, Max) -> [rand:uniform(Max - Min) + Min || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  L = randomElems(1000, 5, 800),
  {Time, _} = timer:tc(Fun1, [L]),
  {Time1, _} = timer:tc(Fun2, [L]),
  io_lib:format("wyniki I. ~p II. ~p ~n", [Time, Time1]).


%%----------Fun------------


myMap(_, []) -> [];
myMap(Fun, [H | T]) -> [Fun(H) | myMap(Fun, T)].

myFilter(_, []) -> [];
myFilter(Fun, [H | T]) -> [ Fun(H) | myFilter(Fun, T)].

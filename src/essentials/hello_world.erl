%%%-------------------------------------------------------------------
%%% @author Mateusz Sokol
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. mar 2018 12:50
%%%-------------------------------------------------------------------
-module(hello_world).
-author("Mateusz Sokol").

%% API
-export([main/0, factorial/1, power/2, fun_check/1, contains/2, duplicateElements/1]).

main() -> io:write(contains([1, 2, 3], 1)).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

power(_, 0) -> 1;
power(N, Power) -> N * power(N, Power - 1).

fun_check(Z) when ((is_float(Z)) and (Z > 3.0)) -> true;
fun_check(_) -> false.

contains([], _) -> false;
contains([Head | _], Head) -> true;
contains([_ | Tail], N) -> contains(Tail, N).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [[H, H] | duplicateElements(T)].

sumFloats([]) -> 0;
sumFloats([H, T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_, T]) -> sumFloats(T).

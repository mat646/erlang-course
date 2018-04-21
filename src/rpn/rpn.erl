%%%-------------------------------------------------------------------
%%% @author Mateusz Sokol
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2018 19:47
%%%-------------------------------------------------------------------
-module(rpn).
-author("Mateusz Sokol").

%% API
-export([main/0, rpn/1]).

main() -> io:fwrite(printResult(rpn("5 6 3 pow 4 / + sqrt"))).

rpn(E) ->
  T = string:tokens(E, " "),
  compute(T, []).

getNumber(E) ->
  F = (catch list_to_float(E)),
  I = (catch list_to_integer(E)),
  if
    is_float(F) -> F;
    is_integer(I) -> I
  end.

printResult(R) when is_float(R) -> float_to_list(R);
printResult(R) when is_integer(R) -> integer_to_list(R).

compute([], []) -> 0;
compute([], [T | _]) -> T;
compute(["+" | T], [A, B | S]) ->
  compute(T, [A + B | S]);
compute(["-" | T], [A, B | S]) ->
  compute(T, [B - A | S]);
compute(["*" | T], [A, B | S]) ->
  compute(T, [A * B | S]);
compute(["/" | T], [A, B | S]) ->
  compute(T, [B / A | S]);
compute(["sin" | T], [A | S]) ->
  compute(T, [math:sin(A) | S]);
compute(["cos" | T], [A | S]) ->
  compute(T, [math:cos(A) | S]);
compute(["tan" | T], [A | S]) ->
  compute(T, [math:tan(A) | S]);
compute(["pow" | T], [A, B | S]) ->
  compute(T, [math:pow(B, A) | S]);
compute(["sqrt" | T], [A | S]) ->
  compute(T, [math:sqrt(A) | S]);
compute([H | T], S) ->
  N = getNumber(H),
  compute(T, [N | S]).

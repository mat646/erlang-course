%%%-------------------------------------------------------------------
%%% @author Mateusz Sokol
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2018 13:05
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Mateusz Sokol").

%% API
-export([main/0, start/0, stop/0, play/1, replyPing/0, replyPong/0]).

main() ->
  start(),
  play(5).

replyPing() ->
  receive
    {-1} ->
      ok;
    {N} ->
      io:write(N),
      pong ! {N-1}
  after
    20000 -> ok
  end.

replyPong() ->
  receive
    {-1} ->
      ok;
    {N} ->
      io:write(N),
      ping ! {N-1}
  after
    20000 -> ok
  end.

start() ->
  Pid1 = spawn(?MODULE, replyPing, []),
  register(ping, Pid1),
  Pid2 = spawn(?MODULE, replyPong, []),
  register(pong, Pid2).

stop() ->
  ping ! {-1},
  pong ! {-1}.

play(N) ->
  ping ! {N}.

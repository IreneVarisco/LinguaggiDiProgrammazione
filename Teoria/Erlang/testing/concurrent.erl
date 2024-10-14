-module(concurrent).

-export([loop/0, rpc/2, start/0]).


start() -> spawn(fun() -> loop() end).
% start() -> register(concurrent, spawn(fun() -> loop() end)).

rpc(Pid, Request) ->
  Pid ! {self(), Request},
  receive
      Response ->
          Response
  end.

loop() ->
  receive
    {From, {msg1, msg2}} ->
      From ! {msg1, msg2},
      loop();
    {From, {msg1, M}} ->
      From ! M,
      loop();
    {From, Other} ->
      From ! {error, Other},
      loop()
  end.

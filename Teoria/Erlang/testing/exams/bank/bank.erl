% This is the central (MAIN) process
-module(bank).
-export([loop/0, start/0]).

Bal=5000
% Each request must be processed independently

start() ->
  group_leader(whereis(user), self()),
  io:format("Bank started~n"),
  loop(1, 1, [], []).
  % register(bank, spawn(fun() -> loop() end)).


loop(Atm, Op) ->
  receive
    {Atm, { balance }} -> io:format("I go a balance request from ATM ~p", [Atm]);
    {Atm, { withdrawal, Amount }} -> io:format("I go a withdraw of ~p from ATM ~p", [Amount, Atm]);
    {Atm, { deposit, Amount }} -> io:format("I go a deposit of ~p from ATM ~p", [Amount, Atm])
  end.


% get_balance
get_balance(Atm, N) -> .

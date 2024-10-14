% remote processes that ask the bank
-module(atm).
-export([]).

%          BANK
%          ATM (ATM ATM ATM)
% { Deposit, Withdraw, Balance }

% Requests from this must be evaluate when they left ATM process
%
% tag each message w/ progressive number local to the issuing ATM
% then push them through a dispatcher (dispatcher.erl) that orders all messages
%
% Initial BALANCE=5000

% Sync operation
balance() -> .

% Async operation
deposit() -> .

% Async operation
withdraw() -> .

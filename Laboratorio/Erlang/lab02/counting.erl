% Exercise 5: Counting Calls. Write a module counting which provides the
% functionality for interacting with a server that counts how many times its
% services has been requested.

% It has to implement several services dummy1, ... dummyn (doesn't matter what
% they do or their real interface) and a service tot that returns a list of
% records indexed on each service (tot included) containing also how many times
% such a service has been requested. Test it from the shell.

-module(counting).

-export([start/0, rpc/2, loop/1]).

update_counts([{S, C} | T], S) -> [{ S, C + 1} | T];
update_counts([H | T], S) -> [H | update_counts(T, S)].

% OLD implementation: trying to implement as an interface for lambda args
% start() ->
%   spawn(counting, loop, [[
%                           {dummy1, 0, fun dummy1/1},
%                           {dummy2, 0, fun dummy2/1},
%                           {dummy3, 0, fun dummy3/1},
%                           {dummy4, 0, fun dummy4/1},
%                           {tot, 0, fun (Pid, L) -> Pid ! L end }
%                          ]]).

% Start server 
start() -> spawn(counting, loop, [[{tot, 0}, {dummy1, 0}, {dummy2, 0}, {dummy3, 0}]]).

rpc(Pid, Service) -> 
   Pid ! { self(), Service },
   receive
     { Pid, Reply } -> Reply
   after(4600) -> exit(timeout)
   end.


% server loop cycle
loop(L) ->
  receive
    {Pid, tot} ->
      Updated = update_counts(L, tot), 
      Pid ! { self(), Updated }, 
      loop(Updated);
    {Pid, dummy1} -> dummy1(Pid), loop(update_counts(L, dummy1));
    {Pid, dummy2} -> dummy2(Pid), loop(update_counts(L, dummy2));
    {Pid, dummy3} -> dummy3(Pid), loop(update_counts(L, dummy3))
  end.
    

dummy1(Pid) -> Pid ! {self(), "dummy1 called !"}.
dummy2(Pid) -> Pid ! {self(), "dummy2 called !"}.
dummy3(Pid) -> Pid ! {self(), "dummy3 called !"}.
% dummy4(Pid) -> Pid ! {self(), "dummy4 called !"}.

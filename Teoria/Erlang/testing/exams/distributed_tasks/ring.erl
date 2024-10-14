-module(ring).
-export([start/2, stop/0, send_message/1, send_message/2, create/3]).


start(NumberProcesses, F) ->
  register(
    ring_recursion_functional,spawn(?MODULE, create, [NumberProcesses, F, self()])),
  receive
    ready -> ok
  after 5000 -> {error, timeout} end.

stop() -> ring_recursion_functional ! {command, stop}.

send_message(Message) ->
  send_message(Message, 1).
send_message(Message, Times) ->
  ring_recursion_functional ! {command, message, [Message, Times]}.

create(1, [H|_], Starter) ->
  Starter ! ready, loop_last(ring_recursion_functional, H);
create(NumberProcesses, [H|TL], Starter) ->
  Next = spawn_link(?MODULE, create, [NumberProcesses-1, TL, Starter]), loop(Next, H).


loop_last(NextProcess, F) ->
  receive
    {command, stop} -> exit(normal),unregister(ring_recursion_functional);
    {command, message, [Message, 1]} ->
      io:format("~p~n", [F(Message)]),
      loop_last(NextProcess, F);
    {command, message, [Message, Times]} ->
      NextProcess ! {command, message, [F(Message), Times-1]},
      loop_last(NextProcess, F) end.

loop(NextProcess, F) ->
  receive
    Msg = {command, stop} ->
      NextProcess ! Msg,
      ok;
    {command, message, [Message, Times]} ->
      NextProcess ! {command, message, [F(Message), Times]},
      loop(NextProcess, F)
  end.

-module(chat_controller).
-export([start/3]).
-import(lib_chan_mm, [send/2]). % the libchan is needed for this project due to privacy

start(MM, _, _) ->
  process_flag(trap_exit, true),
  io:format("chat_controller off we go ...~p~n",[MM]),
  loop(MM).

loop(MM) ->
  receive
    {chan, MM, Msg} ->
      chat_server ! {mm, MM, Msg},
      loop(MM);

    {'EXIT', MM, _Why} ->
      chat_server ! {mm_closed, MM};
    %% when a client connects %% when the session terminates
    Other ->
      io:format("chat_controller unexpected message =~p (MM=~p)~n", [Other, MM]), loop(MM)
  end.

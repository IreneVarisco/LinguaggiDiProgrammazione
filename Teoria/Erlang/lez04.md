<!-- info:start -->
---
    title         : lez04.md
    author        : Kevin
    date          : 29/11/2022
    output        : 
      pdf_document: default
      html_notebook: default
---
<!-- info:end -->

# lez04.md


**IRC lite** &rarr; **I**nternet **R**elay **C**hat

**Client Implementation**

```erlang
  -module(chat_client).
  -export([start/1, connect/5)]).

  start(Nick) -> connect("localhost", 2223, "AsDT67aQ", "general", Nick).

  connect(Host, Port, HostPsw, Group, Nick) ->
    spawn(fun() -> handler(Host, Port, HostPsw, Group, Nick) end).

  handler(Host, Port, HostPsw, Group, Nick) ->
    process_flag(trap_exit, true),
    start_connector(Host, Port, HostPsw),
    disconnected(Group, Nick).

  disconnected(Group, Nick) ->
    receive
      {connected, MM} -> % from the connection process
        io:format("connected to server\nsending data\n"),
        lib_chan_mm:send(MM, {login, Group, Nick}),
        wait_login_response(MM);

      {status, S} -> io:format("~p~n",[S]), disconnected(Group, Nick);
      Other ->
        io:format("chat_client disconnected unexpected:~p~n",[Other]),
        disconnected(Group, Nick)
      end.

  start_connector(Host, Port, Pwd) ->
    % Attention = the line after is done for assign the spawn for parent process
    S = self(), spawn_link(fun() -> try_to_connect(S, Host, Port, Pwd) end).

  try_to_connect(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process case lib_chan:connect(Host, Port, chat, Pwd, []) of
    {error, _Why} ->
      Parent ! {status, {cannot, connect, Host, Port}},
      sleep(2000),
      try_to_connect(Parent, Host, Port, Pwd);
    {ok, MM} ->
      lib_chan_mm:controller(MM, Parent),
      Parent ! {connected, MM}, %% to disconnected exit(connectorFinished)
    end.

  sleep(T) -> receive after T -> true end.
```

# lez03.md

15/11/2022

---

> è difficile per ora in Erlang sapere gestire i problemi

Attraverso la builtin `link`  possiamo collegare un pid per poter monitorare gli invii di
messaggi

```erlang
  -module(dies).
  -export([on_exit/2]).

  on_exit(Pid, Fun) ->
    spawn(fun() ->
      process_flag(trap_exit, true),
      link(Pid),
      receive
        {’EXIT’, Pid, Why} -> Fun(Why)
      end
  end).
```

**System Process** &rarr; questi tipi di processi vengono gestiti da un **flag**
`process_flag(trap_exit, true)`

> ***Links*** are **symmetric**
> + i.e., if A dies, B will sent an exit signal and vice versa
> + to prevent process to die, must be made a system process (`link` builtin function)

> **Monitor** are **asymmetric** link
> + if A monitors B, then B dies, A will be sent an exit signal, but..
> + .. if A dies B **will not** be sent a signal

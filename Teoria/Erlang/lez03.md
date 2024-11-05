# lez03.md

## Gestione degli Errori in Uscita

In Erlang, gestire i problemi derivanti dagli errori tra processi è complesso, specialmente quando i processi sono collegati. Se due processi sono collegati, l'errore di uno può interferire con il comportamento dell'altro.

### Link tra Processi

Tramite la funzione `link` (BIF), è possibile collegare due processi utilizzando il PID, in modo da monitorare l'invio di messaggi di errore tra loro.

> **Nota:** Se uno dei due processi smette di funzionare, l'altro viene notificato con un messaggio del tipo `{EXIT, Pid, Reason}`.

#### Codice di Esempio

```erlang
-module(dies).
-export([on_exit/2]).

on_exit(Pid, Fun) ->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {EXIT, Pid, Why} -> Fun(Why)
        end
    end).
```

Nell'esempio sopra, la funzione `on_exit/2` crea un nuovo processo collegato al PID specificato. Se questo PID termina, viene intercettato il segnale di uscita e gestito con `Fun`.

### Links

I link definiscono un percorso di propagazione degli errori tra due processi. Quando un processo muore, un segnale di uscita viene inviato al processo collegato. L'insieme dei processi collegati a un dato processo è noto come `link set`.

### Exit Signals

I segnali di uscita sono generati quando un processo termina. Sono trasmessi a tutti i processi nel `link set` del processo morente e includono un argomento che spiega il motivo dell'uscita. È anche possibile inviare un segnale di uscita "finto" tramite `exit(Pid, Reason)` senza far terminare il processo mittente.

### System Processes

I processi di sistema ricevono i segnali di uscita come messaggi ordinari, senza terminare a causa loro. Per trasformare un processo in un processo di sistema, si imposta il flag `trap_exit` a `true`.

```erlang
process_flag(trap_exit, true).
```
![pid](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\ERRORE.png)

### Alternative di Gestione dei Crash

Erlang permette diverse modalità di gestione per i processi collegati:

- **Ignorare il crash:** Il processo creato non influenzerà il processo principale.
  ```erlang
  Pid = spawn(fun() -> ... end).
  ```

- **Terminare se il processo creato crasha:** Il processo principale termina se il collegato fallisce.
  ```erlang
  Pid = spawn_link(fun() -> ... end).
  ```

- **Gestire gli errori:** Si usa `trap_exit` per ricevere segnali di errore e gestirli manualmente.
  ```erlang
  process_flag(trap_exit, true),
  Pid = spawn_link(fun() -> ... end).
  ```

Questo è un buon modo per fare house keeping.
---

## Esempio Avanzato: Modulo `edemo1`

Il seguente modulo avvia tre processi con diverse impostazioni di `trap_exit` e monitora l'aliveness dei processi:

```erlang
-module(edemo1).
-export([start/2]).

start(Bool, M) ->
    A = spawn(fun() -> a() end),
    B = spawn(fun() -> b(A, Bool) end),
    C = spawn(fun() -> c(B, M) end),
    sleep(1000), status(b, B), status(c, C).

a() -> process_flag(trap_exit, true), wait(a).
b(A, Bool) -> process_flag(trap_exit, Bool), link(A), wait(b).
c(B, M) -> link(B),
    case M of
        {die, Reason} -> exit(Reason);
        {divide, N} -> 1/N, wait(c);
        normal -> true
    end.
```
```Erlang
wait(Prog) ->
  receive
    Any ->
      io:format("Process ~p received ~p~n", [Prog, Any]),
      wait(Prog)
  end.
sleep(T) ->
  receive
  after T -> true
  end.
status(Name, Pid) ->
  case erlang:is_process_alive(Pid) of
    true -> io:format("process ~p (~p) is alive~n", [Name, Pid]);
    false -> io:format("process ~p (~p) is dead~n", [Name, Pid])
end.
```

### Descrizione del Codice

- **Processo A**: imposta `trap_exit` a `true` per gestire i segnali di uscita.
- **Processo B**: si collega ad A e adotta `trap_exit` se `Bool` è impostato a `true`.
- **Processo C**: si collega a B e può terminare per varie ragioni (`M`).

#### Funzioni di Supporto

1. **wait/1**: Attende messaggi e li stampa.
    ```erlang
    wait(Prog) ->
        receive
            Any -> io:format("Process ~p received ~p~n", [Prog, Any]), wait(Prog)
        end.
    ```

2. **sleep/1**: Pausa il processo per un periodo di tempo specificato.

3. **status/2**: Verifica e stampa se un processo è ancora attivo.

### Esempi di Esecuzione

1. **`edemo1:start(false, {die, normal})`:** B sopravvive alla morte di C, poiché il motivo dell'uscita è `normal`.
2. **`edemo1:start(false, {divide, 0})`:** C causa un errore `{badarith, ...}` che termina B e propaga l'errore ad A.
3. **`edemo1:start(false, {die, abc})`:** Il segnale di uscita `{EXIT, Pid, abc}` viene intrappolato e gestito.

---

## Monitoraggio dei Processi con `erlang:monitor/2`

I monitor forniscono una relazione unidirezionale tra processi, utile per ricevere notifiche sulla terminazione senza creare un collegamento simmetrico.

### Descrizione del Monitor

Se un processo A monitora B e B termina, A riceverà un messaggio `{DOWN, Ref, process, B, Reason}`. Tuttavia, se A termina, B non viene influenzato.

```erlang
erlang:monitor(process, B).
```

### Vantaggi del Monitor

I monitor sono ideali per scenari in cui è necessario rilevare errori senza rischiare di interrompere il processo monitorato.
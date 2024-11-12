
# lez04.md

## Programmazione Distribuita

La programmazione distribuita è un metodo di sviluppo software in cui il calcolo viene eseguito contemporaneamente su più nodi, spesso remoti. Ciascun nodo esegue una parte del programma e comunica con gli altri nodi mentre lavora verso un obiettivo comune.

Un esempio di programmazione distribuita si può trovare nei giochi online e nelle applicazioni di chat.


### qualità di un programma distribuito
- **Performance** i programmi sono più veloci arrangiando le diverse parti del prgramma che vengono eseguiti su macchine diverse

- **Affidabilità** tolleranza ai guasti dato chè è strutturato in modo che il sistema sia replicato su più macchine: se un calcolo fallisce continua su un'altra macchina.

- **Scalabilità** le risorse su una singola macchina spesso esauriscono, aggiungere una macchina vuol dire raddoppiare le risorse

- **Applicazioni intrinsecamente distribuite**

### Distributed Erlang
Le applicazioni sono eseguite su un insieme di computer strettamente accoppiati **Erlang nodes**
I processi possono essere generati su ogni nodo e le cose funzionano come sempre.

### Socket-Based Distribution
La distinzione tra i 2 è la sicurezza.
può essere eseguito in un ambiente non affidabile, è meno potente e c'è un controllo dettagliato su cosa può essere eseguito su un nodo.

```erlang
-module(kvs).
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).
store(Key, Value) -> rpc({store, Key, Value}).
lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->
  kvs ! {self(), Q},
  receive
    {kvs, Reply} -> Reply
  end.
loop() ->
  receive
    {From, {store, Key, Value}} -> put(Key, {ok, Value}), From ! {kvs, true}, loop();
    {From, {lookup, Key}} -> From ! {kvs, get(Key)}, loop()
end.
```
#### Spiegazione

1. Avvio:
start() -> register(kvs, spawn(fun() -> loop() end)).:
register(kvs, ...): Registra il processo appena creato con il nome kvs.
spawn(fun() -> loop() end): Crea un nuovo processo che eseguirà la funzione loop.

2. Memorizzazione e Recupero di Coppie Chiave-Valore:
store(Key, Value) -> rpc({store, Key, Value}).:
Invia una richiesta RPC (Remote Procedure Call) al processo kvs per memorizzare una coppia chiave-valore.
lookup(Key) -> rpc({lookup, Key}).:
Invia una richiesta RPC al processo kvs per recuperare il valore associato a una chiave.

3. Gestione delle Richieste e Risposte:
rpc(Q) -> ...:
Invia la richiesta Q al processo kvs.
Attende una risposta dal processo kvs e la restituisce.

4. Ciclo Principale del Processo Magazzino:
loop() -> ...:
È il cuore del processo. Riceve continuamente messaggi.
Se il messaggio è una richiesta di memorizzazione (store):
Memorizza la coppia chiave-valore internamente (probabilmente usando una mappa o un dizionario).
Invia una conferma al mittente.
Se il messaggio è una richiesta di recupero (lookup):
Cerca il valore associato alla chiave e lo invia al mittente.
Richiama ricorsivamente loop per continuare a processare le richieste.

![pid](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\programmazionedistribuita.png)


### Node
è una VM di Erlang ed è autonoma: ha suo spazio di indirizzi e il suo insieme di processi.
l'accesso a un singolo nodo è protetto da un sistema di cookie.
ogni nodo ha un cookie e deve essere uguale a qualsiasi nodo con cui comunica.
il cookie viene impostato all'avvio dalla VM o attraverso `erlang:set_cookie`
**cluster**: insieme di nodi con lo stesso cookie.

### comandi Primitivi

- spawn(Node, Mod, Func, ArgList)->Pid
- spawn_link(Node, Mod, Func, ArgList)->Pid
- disconnect_node(Node)->bools() | ignored
- monitor_node(Node, Flag)->true
- {RegName, Node}!Msg

## Socket Based Distribution: lib_chan 
 utilizza una forma limitata di spawn in cui il propretario di una macchina ha il controllo solo su ciò che viene eseguito sulla sua macchina.

`lib_chan` &rarr;  modulo che permette l'utente du controllare esplicitamente quali processi sono spawnati sulle sue macchine

- start_server()->true
fa partire un server su localhost, il suo comportamento dipende da $HOME/ .erlang_config/lib_chan.conf
- connect(Host, Port, S, P, ArgsC)->{ok, Pid}|{error, Why}
prova ad aprire òa porta Port su Host e dopo ad attivare il servizio S protetto da password P.
The configuration file contains tuples of the form:
- {port, NNNN}
inizia ad ascoltare dal port number NNNN
- {service, S, password, P, mfa, SomeMod, SomeFunc, SomeArgs}
questo definisce S, quando la connessione e creata dalla connect call il server spawna
- SomeMod:SomeFunc(MM, ArgC, SomeArgs)
dove MM è il Pid di un processo proxy che manda un messaggio a un cliente e ArgsC viene dalla client connect call.

```erlang
{port, 12340}.
{service, nameServer, password, "ABXy45", mfa, mod_name_server, start_me_up, notUsed}.
```
```erlang
-module(mod_name_server).
-export([start_me_up/3]).
start_me_up(MM, _ArgsC, _ArgS) -> loop(MM).
loop(MM) ->
receive
{chan, MM, {store, K, V}} -> kvs:store(K,V), loop(MM);
{chan, MM, {lookup, K}} -> MM ! {send, kvs:lookup(K)}, loop(MM);
{chan_closed, MM} -> true
end.
```
esecuzione:

```1> kvs:start().
true
2> lib_chan:start_server().
Starting a port server on 12340...
true
3> kvs:lookup(joe).
{ok,"writing a book"}
```
```
1> {ok, Pid} = lib_chan:connect("localhost", 12340, nameServer, "ABXy45", "").
{ok, <0.43.0>}
2> lib_chan:cast(Pid, {store, joe, "writing a book"}).
{send,{store,joe,"writing a book"}}
3> lib_chan:rpc(Pid, {lookup, joe}).
{ok,"writing a book"}
4> lib_chan:rpc(Pid, {lookup, jim}).
undefined
```

## Esempio di spawning distribuito
Questo codice Erlang fornisce un meccanismo per eseguire funzioni remotamente su un altro nodo della rete Erlang. Un processo remoto viene avviato sul nodo di destinazione, e le richieste RPC vengono inviate a questo processo. Il processo remoto esegue la funzione richiesta e restituisce il risultato al mittente. Questo pattern è comunemente utilizzato per distribuire il carico di lavoro tra diversi nodi, offloadare operazioni costose o accedere a risorse remote.

```erlang
-module(ddemo).
-export([rpc/4, start/1]).

start(Node) -> spawn(Node, fun() -> loop() end).

rpc(Pid, M, F, A) ->
  Pid ! {rpc, self(), M, F, A},
  receive
    {Pid, Response} -> Response
  end.
loop() ->
  receive
    {rpc, Pid, M, F, A} ->
      Pid ! {self(), (catch apply(M, F, A))},
      loop()
  end.
```
Avvio del Processo Remoto:

**start(Node) -> spawn(Node, fun() -> loop() end).**:

- **spawn(Node, fun() -> loop() end)**: Spawna un nuovo processo sul nodo specificato (Node) che eseguirà la funzione loop.
Invio di Richieste RPC:

- **Pid ! {rpc, self(), M, F, A}**: Invia un messaggio al processo remoto con la richiesta RPC, specificando il mittente (self()), il modulo (M), la funzione (F) e gli argomenti (A).

- **receive ... end**: Attende la risposta dal processo remoto e la restituisce.
Gestione delle Richieste RPC nel Processo Remoto:

- **loop() -> ...** :
Riceve continuamente messaggi.
Se il messaggio è una richiesta RPC:
Esegue la funzione specificata (apply(M, F, A)) utilizzando catch per gestire eventuali eccezioni.
Invia la risposta al mittente.
Ritorna al ciclo loop per ricevere la prossima richiesta.

### Esecuzione:
```cmd
[19:01]cazzola@surtur:~/lp/erlang>erl -name sif -setcookie abc
(sif@surtur.di.unimi.it)1> Pid = ddemo:start(’amora@thor.di.unimi.it’).
<8745.43.0>
(sif@surtur.di.unimi.it)3> ddemo:rpc(Pid, erlang, node, []).
’amora@thor.di.unimi.it’
```
Erlang fornisce librerie specifiche con supporto per `rpc` e `global` distribuiti.

## The Cookie Protection System
&rarr; due nodi che comunicano DEVONO avere lo stesso magic cookie.

### 3 modi per settare il cookie

1. immagazzinare il cookie in $HOME/.erlang.cookie
```cmd
[19:26]cazzola@surtur:~/lp/erlang>echo "A Magic Cookie" > ~/.erlang.cookie
[19:27]cazzola@surtur:~/lp/erlang>chmod 400 ~/.erlang.cookie
```
2. opzione `-setcookie`(il meno sicuro)
```
[19:27]cazzola@surtur:~/lp/erlang>erl -setcookie "A Magic Cookie"
```
3. BIF erlang:set_cookies
```
[19:34]cazzola@surtur:~/lp/erlang>erl -sname sif
(sif@surtur)1> erlang:set_cookie(node(), ’A Magic Cookie’).
true
```

### problema del spawn-based distribution: 
  è perfetto per quando possiedi tutte le macchine e vuoi controllarle da una singola macchina ma non va bene per quando sono più persone a possedere le macchine: il cliente può spawnare qualsiasi processo nella macchina server &rarr; in questo caso è meglio socket-base distribution
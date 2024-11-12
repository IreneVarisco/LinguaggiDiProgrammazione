# Lez05


## **IRC lite** &rarr; **I**nternet **R**elay **C**hat
IRC lite rappresenta un canale di comunicazione, simile a piattaforme come Discord o Reddit, dove gli utenti possono connettersi e inviare messaggi. Ogni messaggio è broadcastato a tutti i membri del canale, offrendo una forma di comunicazione di gruppo.

### Funzionalità principali
- Gestione dei gruppi: ogni volta che viene creato un canale da un utente, i nuovi membri si collegano automaticamente al gruppo associato.
- Server e client: il server è eseguito su una macchina dedicata, mentre i clienti inviano e ricevono messaggi tramite un 'group controller'.
- Group controller: gestisce un singolo gruppo; ogni messaggio inviato al controller viene broadcastato a tutti i membri.
- Middle-man: si occupa del trasporto dei dati, astrattamente dai socket.

### Diagramma del sistema
![Diagramma IRC](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\IRC.png)


### **Client Implementation**

Il client viene eseguito come processo di sistema, generando un processo di connessione per tentare di connettersi al server. Dopo la connessione, il client passa dallo stato di 'disconnected' a 'connected'.

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

    % stato di disconnessione
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
    % Avvia il processo di connessione al server e passa al processo genitore
  start_connector(Host, Port, Pwd) ->
    % Attention = the line after is done for assign the spawn for parent process
    % S= self()... è diverso da spawn_link...
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
- start(Nick) avvio client, tenta di connettersi al server con un nickname
- handler(Host, Port, HostPsw, Group, Nick) gestione della connessione e della sessione del client
- start_client() serve per avviare il processo di connessione
- {error, _Why} In caso di errore, stampa la causa e termina il tentativo di connessione
- {ok, MM} Se la connessione ha successo, il client è pronto a ricevere e inviare messaggi


```erlang
% Attende la risposta di login del server
wait_login_response(MM) ->
    receive
        {chan, MM, ack} -> active(MM);  % Risposta di conferma di login
        {'EXIT', _Pid, connectorFinished} -> wait_login_response(MM);
        Other ->
            io:format("chat_client login unexpected:~p~n",[Other]),
            wait_login_response(MM)
    end.

% Stato attivo: gestisce l'invio e la ricezione di messaggi
active(MM) ->
    receive
        {msg, Nick, Str} ->
            lib_chan_mm:send(MM, {relay, Nick, Str}),  % Invia un messaggio al gruppo
            active(MM);
        {chan, MM, {msg, From, Pid, Str}} ->
            io:format("~p@~p: ~p~n", [From,Pid,Str]),  % Ricezione di un messaggio
            active(MM);
        {close, MM} -> exit(serverDied);
        Other ->
            io:format("chat_client active unexpected:~p~n",[Other]),
            active(MM)
    end.
```
- `active` manda messaggi al gruppo e viceversa, inoltre monitora la connessione col gruppo

### **Server Implementation** chat controller

```erlang
% Configurazione della porta e del servizio
{port, 2223}.
{service, chat, password,"AsDT67aQ",mfa,chat_controller,start,[]}.

-module(chat_controller).
-export([start/3]).
-import(lib_chan_mm, [send/2]).

% Funzione di avvio del controller della chat
start(MM, _, _) ->
    process_flag(trap_exit, true),
    io:format("chat_controller off we go ...~p~n",[MM]),
    loop(MM).

% Loop principale del controller
loop(MM) ->
    receive
        {chan, MM, Msg} ->  % Quando un client si connette
            chat_server ! {mm, MM, Msg},
            loop(MM);
        {'EXIT', MM, _Why} ->  % Quando la sessione termina
            chat_server ! {mm_closed, MM};
        Other ->
            io:format("chat_controller unexpected message =~p (MM=~p)~n", [Other, MM]),
            loop(MM)
    end.
```

### **Server Implementation** chat server

```erlang
-module(chat_server).

% Avvia il server con configurazione
start() -> start_server(), lib_chan:start_server("chat.conf").

% Funzione di avvio del server e del loop principale
start_server() ->
    register(chat_server,
    spawn(fun() ->
        process_flag(trap_exit, true),
        Val = (catch server_loop([])),
        io:format("Server terminated with:~p~n",[Val])
    end)).

% Loop principale del server
server_loop(L) ->
    receive
        {mm, Channel, {login, Group, Nick}} ->
            case lookup(Group, L) of
                {ok, Pid} -> Pid ! {login, Channel, Nick}, server_loop(L);
                error ->
                    Pid = spawn_link(fun() -> chat_group:start(Channel, Nick) end),
                    server_loop([{Group,Pid}|L])
            end;
        {mm_closed, _} -> server_loop(L);
        {'EXIT', Pid, allGone} -> L1 = remove_group(Pid, L), server_loop(L1);
        Msg -> io:format("Server received Msg=~p~n", [Msg]), server_loop(L)
    end.

% Funzioni di ricerca e rimozione gruppo
lookup(G, [{G,Pid}|_]) -> {ok, Pid};
lookup(G, [_|T]) -> lookup(G, T);
lookup(_,[]) -> error.

remove_group(Pid, [{G,Pid}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T]) -> [H|remove_group(Pid, T)];
remove_group(_, []) -> [].
```

### **Server Implementation** group manager

```erlang
-module(chat_group).
-export([start/2]).

% Inizia un gruppo e invia un messaggio di avvio
start(C, Nick) ->
    process_flag(trap_exit, true),
    lib_chan_mm:controller(C, self()), lib_chan_mm:send(C, ack),
    self() ! {chan, C, {relay, Nick, "I’m starting the group"}},
    group_controller([{C,Nick}]).

% Funzione per eliminare un membro dal gruppo
delete(Pid, [{Pid,Nick}|T], L) -> {Nick, lists:reverse(T, L)};
delete(Pid, [H|T], L) -> delete(Pid, T, [H|L]);
delete(_, [], L) -> {"????", L}.

% Loop del controller del gruppo
group_controller([]) -> exit(allGone);
group_controller(L) ->
    receive
        {chan, C, {relay, Nick, Str}} ->
            lists:foreach(fun({Pid,_}) -> lib_chan_mm:send(Pid, {msg,Nick,C,Str}) end, L),
            group_controller(L);
        {login, C, Nick} ->
            lib_chan_mm:controller(C, self()), lib_chan_mm:send(C, ack),
            self() ! {chan, C, {relay, Nick, "I’m joining the group"}},
            group_controller([{C,Nick}|L]);
        {chan_closed, C} ->
            {Nick, L1} = delete(C, L, []),
            self() ! {chan, C, {relay, Nick, "I’m leaving the group"}},
            group_controller(L1);
        Any ->
            io:format("group controller received Msg=~p~n", [Any]),
            group_controller(L)
    end.
```

### Output

```cmd
1> chat_server:start().
lib_chan starting:"chat.conf"
ConfigData=[{port,2223}, {service,chat,password,"AsDT67aQ",mfa,chat_controller,start,[]}]
chat_controller off we go ...<0.39.0>
chat_controller off we go ...<0.41.0>
chat_controller off we go ...<0.43.0>
server error should die with exit(normal) was:{mm_closed,<0.39.0>}
chat_controller off we go ...<0.46.0>
server error should die with exit(normal) was:mm_closed,<0.46.0>}
server error should die with exit(normal) was:mm_closed,<0.41.0>}
server error should die with exit(normal) was:mm_closed,<0.43.0>}
```
- chat_server:start().: Avvia il server di chat.
- Configurazione di lib_chan: Viene caricata la configurazione (chat.conf) con dati specifici (porta 2223, password AsDT67aQ, servizio chat, modulo chat_controller).
- Processi del Controller di Chat: Sono visualizzati messaggi di log per ciascun processo chat_controller (identificati da <0.x.0>), che indicano la gestione dei canali di comunicazione. I processi si connettono e vengono chiusi normalmente o con un'uscita segnalata come mm_closed.

```cmd
1> ChatDaemon = chat_client:start(walter).
walter@<0.41.0>: "I’m joining the group"
’walter cazzola’@<0.43.0>: "I’m joining the group"
2> ChatDaemon ! {msg, walter, "Hello World!!!"}.
{msg,walter,"Hello World!!!"}
walter@<0.41.0>: "Hello World!!!"
’walter cazzola’@<0.43.0>: "Hello Walter!!!"
cazzola@<0.39.0>: "Hello Walter!!!"
cazzola@<0.39.0>: "I’m leaving the group"
cazzola@<0.46.0>: "I’m joining the group"
cazzola@<0.46.0>: "I’m leaving the group"
```
#### Creazione di ChatDaemon con il nome walter:

- chat_client:start(walter) avvia un client di chat con nickname walter.
- Messaggi "I'm joining the group": Conferma che walter si unisce al gruppo.
#### Invio di un Messaggio da walter:

- ChatDaemon ! {msg, walter, "Hello World!!!"}: Viene inviato un messaggio "Hello World!!!" da walter, che viene ricevuto e visualizzato.
Risposta con il Messaggio "Hello Walter!!!":

#### Messaggi di Uscita dal Gruppo:

I client walter e cazzola mostrano messaggi come "I'm leaving the group", indicativo della loro disconnessione dal gruppo.

```cmd
1> ChatDaemon = chat_client:start(’walter cazzola’).
’walter cazzola’@<0.43.0>: "I’m joining the group"
walter@<0.41.0>: "Hello World!!!"
2> ChatDaemon!{msg,’walter cazzola’,"Hello Walter!!!"}.
{msg,’walter cazzola’,"Hello Walter!!!"}
’walter cazzola’@<0.43.0>: "Hello Walter!!!"
cazzola@<0.39.0>: "Hello Walter!!!"
cazzola@<0.39.0>: "I’m leaving the group"
cazzola@<0.46.0>: "I’m joining the group"
cazzola@<0.46.0>: "I’m leaving the group"
walter@<0.41.0>: "I’m leaving the group"
```
#### Avvio del Client walter cazzola:

- chat_client:start('walter cazzola') avvia un client con nome 'walter cazzola'.
Messaggi di Ingresso nel Gruppo: 'walter cazzola' si unisce al gruppo.

#### Scambio di Messaggi:

- ChatDaemon ! {msg, "walter cazzola", "Hello Walter!!!"} invia "Hello Walter!!!" e mostra la ricezione del messaggio da parte di altri partecipanti, come cazzola.
Messaggi di Uscita:

Messaggi di uscita dal gruppo mostrano che i client come walter e cazzola lasciano il gruppo, segnalando la chiusura delle loro sessioni.

```cmd
1> ChatDaemon = chat_client:start(cazzola).
cazzola@<0.39.0>: "I’m starting the group"
walter@<0.41.0>: "I’m joining the group"
’walter cazzola’@<0.43.0>: "I’m joining the group"
walter@<0.41.0>: "Hello World!!!"
’walter cazzola’@<0.43.0>: "Hello Walter!!!"
2> ChatDaemon ! {msg, cazzola, "Hello Walter!!!"}.
{msg,cazzola,"Hello Walter!!!"}
cazzola@<0.39.0>: "Hello Walter!!!"
3> ^C [21:35]cazzola@surtur:~/lp/erlang/chat>erl
1> ChatDaemon = chat_client:start(cazzola).
cazzola@<0.46.0>: "I’m joining the group"
```
#### Avvio del Client cazzola:

chat_client:start(cazzola) avvia un nuovo client con nome cazzola.
#### Messaggi di Ingresso: 
cazzola si unisce e annuncia "I'm starting the group".
#### Scambio di Messaggi:

- Invio di Messaggi tra i Client: cazzola e altri membri inviano e ricevono messaggi di benvenuto come "Hello Walter!!!" e "I'm joining the group".

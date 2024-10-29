# Lez02.md

Erlang è un linguaggio progettato per applicazioni distribuite e concorrenti. Uno degli aspetti più distintivi è la gestione della **concorrenza** attraverso il **Model Actor**, evitando la condivisione dello stato tra processi.

## Concorrenza Tradizionale vs Actor Model

`Concorrenza`: caratteristica dei sistemi di elaborazione nei quali può verificarsi che un insieme di processi o sottoprocessi (thread) computazionali sia in esecuzione nello stesso istante.

`Thread`: sottoprocessiche perano sulla **memoria condivisa**, modo per gestire la concorrenza. processo leggero, non hanno la parte dati.

Ci possono essere problemi, ad esempio:
- Race conditions con perdita di aggiornamenti: ondizioni di concorrenza che portano a errori casuali.

![race-condition](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\race-condition.png)

- Deadlocks: situazioni in cui due o più processi si bloccano a vicenda.
![deadlocks](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\deadlocks.png)


> ErLang utilizza al posto dei thread il **modello degli attori** 

L'Actor Model permette a ogni attore di inviare messaggi, creare nuovi attori, e cambiare comportamento in base ai messaggi ricevuti. Le caratteristiche principali sono:
- **Assenza di Stato Condiviso**: non esiste condivisione diretta dello stato tra gli attori.
- **Comunicazione Asincrona**: gli attori inviano e ricevono messaggi senza attendere la ricezione.

Tutto ciò che ha la prima lettera maiuscola è una variabile, altrimenti è un atomo.

Erlang è in grado di gestire milioni di attori senza crearea problemi alla macchina su cui sta girando.

Ogni oggetto è un **attore**
> + ha una casella di posta ed un comportamento
> + gli attori comunicano tramite uno scambio di messaggi bufferato nella mailbox
> + vengono eseguiti contemporaneamente e sono implementati come thread leggeri dello spazio utente
> + gli attori sono delle funzioni, non hanno uno stato condiviso

la computazione è data-driven 
 > dopo aver ricevunto un messaggio l'attore può mandare una serie di messaggi ad altri attori, reare un certo numero di attori, decidere di utilizzare un comportamento diverso  per gestire il prossimo messaggio nella mailbox.

**Non c'è nessuna garanzia che i messaggi siano inoltrati ed arrivino a destinazione**


**ATTORI**

+ `spawn()        ` Costruzione di attore
```erlang
Pid = spawn(Modulo, Funzione, [Argomenti])
```
esempio:
```erlang
Pid = spawn(demo, loop, [3, a]).
```
Il comando crea un nuovo processo che esegue `loop/2` nel modulo `demo`.
+ `!              ` Invio di un messaggio
+ `<pattern-match>` Ricezione di un messaggio

> pid <nodo_macchina>.<current_pid>.<non_usato>

Esempio
  : `pid < 0.36.0 >`

  ![pid](C:\Users\Irne\Desktop\GIGI\uni\LinguaggiDiProgrammazione\Teoria\Erlang\pid.png)



Il figlio conosce il proprio `pid` ma non conosce quello del padre.
Mentre il padre conosce il `pid` del padre! 
pid 0.37.0 fa cose diverse dall'altro

```ErLang
-module(processes_demo).
-export([start/2, loop/2]).
start(N,A) -> spawn (processes_demo, loop, [N,A]).
loop(0,A) -> io:format("~p(~p) ~p~n", [A, self(), stops]);
loop(N,A) -> io:format("~p(~p) ~p~n", [A, self(), N]), loop(N-1,A).
```

questo codice Erlang crea un processo che conta all'indietro da un numero iniziale N fino a 0, stampando un messaggio ad ogni passo. Il processo continua a funzionare fino a quando il contatore raggiunge 0.

- Modulo:

`-module(processes_demo).`: Definisce un nuovo modulo chiamato processes_demo. Un modulo in Erlang è come un contenitore per funzioni e variabili correlate.

- Funzioni esportate:

`-export([start/2, loop/2]).`: Indica che le funzioni start e loop sono disponibili per essere chiamate da altri moduli. Il numero dopo il nome della funzione (es. /2) indica il numero di argomenti che la funzione accetta.

- Funzione start:

`start(N,A) -> spawn (processes_demo, loop, [N,A]).`: Questa funzione crea un nuovo processo (grazie a spawn) che eseguirà la funzione loop.

`N`: Il numero iniziale da cui iniziare il conteggio all'indietro.

`A`: Un valore che viene passato al processo e che non viene modificato durante l'esecuzione.

- Funzione loop:

`loop(0,A) -> io:format("~p(~p) ~p~n", [A, self(), stops]).`: Questa è la condizione di arresto. Quando il contatore N raggiunge 0, la funzione stampa un messaggio contenente:

`A`: Il valore passato inizialmente.

`self()`: L'ID del processo corrente. restituisce il PID del processo.

`stops`: Una stringa che indica che il processo si sta arrestando.

`loop(N,A) -> io:format("~p(~p) ~p~n", [A, self(), N]), loop(N-1,A).`: Questa è la parte ricorsiva della funzione. Ad ogni iterazione:
Stampa un messaggio con il valore corrente di N, l'ID del processo e il valore di A.
Chiama se stessa con N-1, decrementando così il contatore.

- In sintesi:

1. Viene creato un nuovo processo tramite la funzione start.
2. Il nuovo processo esegue la funzione loop.
3. La funzione loop stampa un messaggio e decrementa il contatore ad ogni iterazione.
4. Quando il contatore raggiunge 0, il processo si arresta.

- Concetti chiave di Erlang utilizzati:

`Processi`: Erlang è un linguaggio concorrente, quindi può eseguire più processi contemporaneamente.
Pattern matching: Il modo in cui Erlang seleziona quale parte del codice eseguire in base ai valori degli argomenti.

`Ricorsione`: La funzione loop si chiama ripetutamente fino a raggiungere una condizione di arresto.

`IO`: La funzione io:format viene utilizzata per stampare messaggi sulla console.

Cosa succede quando si esegue questo codice?

Se chiami la funzione start con dei valori specifici per N e A, vedrai una serie di messaggi stampati sulla console, ognuno dei quali mostra il valore corrente del contatore, l'ID del processo e il valore di A. Al termine del conteggio, verrà stampato un messaggio finale che indica l'arresto del processo.

Esempio:

```Erlang
processes_demo:start(5, "Hello").
```

Questo codice creerà un processo che conterà all'indietro da 5 a 0, stampando messaggi simili a questo:

```cmd
Hello(PID<0.32.0>) 5
Hello(PID<0.32.0>) 4
Hello(PID<0.32.0>) 3
...
Hello(PID<0.32.0>) stops
```

ogni attore è caratterizzato da:
- un indirizzo che lo identifica
- una mailbox dove vengono immagazzinati i messaggi mandati ma non puliti. vengono ordinati per orario di arrivo.

## Invio di un messaggio:
 - bisogna conoscere il PID dell'attore desitnatario
 - bisogna mandare il proprio pid al destinatario se è richiesta una risposta
 - bisogna usare ! primitivo.

 ```erlang
Pid ! Messaggio
```

## Ricizione di un messaggio:
- si basa su pattern matching
- funzione bloccante
- rimango in attesa di un valore quando ho bisogno di quel valore per andare avanti.
- i messaggi vengono processati nell'ordine di arrivo
```erlang
receive
    {Mittente, Contenuto} ->
        %% Azione con il messaggio
end.
```

## Esempi di Utilizzo del Model Actor

### Calcolo delle Aree
Il seguente modulo calcola l'area di un rettangolo e un cerchio:
```erlang
-module(area_server).
-export([loop/0]).

loop() ->
    receive
        {rectangle, Width, Ht} ->
            io:format("Area of rectangle is ~p~n", [Width * Ht]),
            loop();
        {circle, R} ->
            io:format("Area of circle is ~p~n", [3.14159 * R * R]),
            loop();
        _Other ->
            io:format("Unknown message"),
            loop()
    end.
```

### Conversione delle Temperature
Il modulo seguente converte temperature tra Celsius e Fahrenheit:
```erlang
-module(converter).
-export([t_converter/0]).

t_converter() ->
    receive
        {toF, C} -> io:format("~p °C is ~p °F~n", [C, 32 + C * 9 / 5]), t_converter();
        {toC, F} -> io:format("~p °F is ~p °C~n", [F, (F - 32) * 5 / 9]), t_converter();
        {stop} -> io:format("Stopping~n");
        _ -> t_converter()
    end.
```

## Funzionalità Avanzate del Model Actor

### Scheduling degli Attori
Erlang utilizza un **scheduler preemptive** all'interno del runtime BEAM. Gli attori possono essere sospesi e reinseriti in coda di esecuzione quando:
- Viene superato un tempo di esecuzione definito.
- Entra una dichiarazione `receive` senza messaggi disponibili.

### Simmetria di Multi-Processing (SMP)
Il runtime BEAM supporta l’esecuzione parallela sui processori multipli del sistema, garantendo che applicazioni in Erlang possano sfruttare l’hardware multicore.

### Misurazione del Tempo di Creazione degli Attori
Il modulo `processes` mostra come misurare il tempo necessario per generare un certo numero di processi:
```erlang
-module(processes).
-export([max/1]).

max(N) ->
    Max = erlang:system_info(process_limit),
    io:format("Maximum allowed processes: ~p~n", [Max]),
    statistics(runtime), statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime), {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    io:format("Process spawn time = ~p microseconds~n", [Time1 * 1000 / N]).

wait() ->
    receive die -> void end.
```

```cmd
1> processes:max(20000).
Maximum allowed processes:32768
Process spawn time = 2.5 (3.4) microseconds
ok
2> processes:max(40000).
Maximum allowed processes:32768
=ERROR REPORT==== 8-Nov-2011::14:24:32 ===
Too many processes
...
[16:48]cazzola@surtur:~/lp/erlang>erl +P 100000
1> processes:max(50000).
Maximum allowed processes:100000
Process spawn time = 3.2 (3.74) microseconds
ok
```

### Assegnazione di Nomi agli Attori
Erlang permette di registrare gli attori con un nome pubblico. In pratica creiamo un alias.
La sintassi è la seguente:
```erlang
register(nome, Pid)
unregister(nome)
whereis(nome) -> Pid | undefined
```

Esempio di modulo `clock` che assegna il nome "clock" a un attore:
```erlang
-module(clock).
-export([start/2, stop/0]).

start(Time, Fun) -> register(clock, spawn(fun() -> tick(Time, Fun) end)).
stop() -> clock ! stop.

tick(Time, Fun) ->
    receive
        stop -> void;
    after
        Time -> Fun(), tick(Time, Fun)
    end.
```

```cmd
5> clock:start(5000, fun() -> io:format("TICK ~p~n",[erlang:now()]) end).
true
TICK 1320,769016,673190
TICK 1320,769021,678451
TICK 1320,769026,679120
7> clock:stop().
stop
```



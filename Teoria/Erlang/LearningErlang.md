
# Imparare Erlang

## Introduzione a Erlang

Erlang è un linguaggio di programmazione funzionale e concorrente progettato per costruire sistemi scalabili e tolleranti ai guasti. È stato sviluppato da Ericsson per supportare sistemi distribuiti e applicazioni di telecomunicazione. Eccelle nella gestione dei processi paralleli e offre un forte supporto per la concorrenza e la tolleranza ai guasti.

### Caratteristiche principali:
- **Linguaggio funzionale**: Erlang è un linguaggio funzionale, il che significa che le funzioni sono i principali blocchi costitutivi del codice.
- **Concorrenza**: Erlang è stato progettato fin dall'inizio per la concorrenza, rendendolo ideale per sistemi distribuiti e in tempo reale.
- **Tolleranza ai guasti**: Le applicazioni Erlang sono progettate per continuare a funzionare anche quando parti del sistema falliscono.
- **Dati immutabili**: I dati in Erlang sono immutabili, il che significa che una volta creati non possono essere modificati.
- **Processi leggeri**: I processi in Erlang sono estremamente leggeri e possono essere eseguiti milioni di volte contemporaneamente.

## Sintassi e costrutti di base

### 1. Variabili
Le variabili in Erlang iniziano con una lettera maiuscola e sono immutabili.

```erlang
X = 10,
Y = X + 20.
```

### 2. Funzioni
Le funzioni sono definite utilizzando la parola chiave `fun` o all'interno di moduli. Erlang supporta sia funzioni nominate che anonime.

```erlang
% Funzione anonima
Add = fun(X, Y) -> X + Y end.

% Funzione nominata all'interno di un modulo
-module(math).
-export([add/2]).

add(X, Y) ->
    X + Y.
```

### 3. Moduli
Il codice in Erlang è organizzato in moduli. Un modulo è una raccolta di funzioni.

```erlang
-module(my_module).
-export([my_function/0]).

my_function() ->
    io:format("Ciao, Erlang!~n").
```

### 4. Pattern Matching
Erlang utilizza il pattern matching in modo estensivo, rendendolo una parte fondamentale del linguaggio. Il pattern matching viene utilizzato per associare variabili ai valori e può essere utilizzato anche per il controllo del flusso.

```erlang
{X, Y} = {5, 10}.
```

### 5. Ricorsione
Erlang si basa sulla ricorsione per i cicli, poiché non ci sono costrutti di ciclo tradizionali come `for` o `while`.

```erlang
% Funzione fattoriale
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
```

### 6. Concorrenza
La concorrenza in Erlang viene realizzata utilizzando i processi. Ogni processo è leggero e funziona in modo indipendente.

```erlang
% Creazione di un nuovo processo
Pid = spawn(fun() -> io:format("Questo è un nuovo processo!~n") end).
```

### 7. Passaggio di messaggi
I processi in Erlang comunicano tramite il passaggio di messaggi.

```erlang
% Inviare un messaggio
Pid ! {ciao, mondo}.

% Ricevere un messaggio
receive
    {ciao, Mondo} ->
        io:format("Messaggio ricevuto: ~p~n", [Mondo])
end.
```



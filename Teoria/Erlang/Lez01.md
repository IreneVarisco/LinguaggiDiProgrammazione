# Lez01.md

## Erlang


> È un linguaggio general purpose anche se è più propenso per le **telecomunicazioni**

> È "concurrency oriented", quindi i processi sono la base di ogni computazione

> è un linguaggio molto usato sopratutto nella rete


> linguaggio funzionale


sono tutti peer quindi puoi sia ricevere che mandare messaggi(assomiglia alla posta elettronica)

```erlang
-module(fact).
-export([fact/1]).
fact(0) -> 1;
fact(N) -> N*fact(N-1).
```
è necessario utilizzare lo shell BEAM
### Caratteristiche principali:

- **Concorrenza**: 
Erlang è orientato alla concorrenza, adotta il modello degli attori:
  – scambio di messaggi asincrono; 
  – memoria non condivisa

sono tutti peer quindi puoi sia ricevere che mandare messaggi(assomiglia alla posta elettronica)

- **Tolleranza ai guasti**: Supporta la distribuzione e aggiornamenti dinamici (hot-swapping).

- **Tipizzazione dinamica**: È un linguaggio a tipi dinamici.

- **Scalabilità**: Progettato per eseguire milioni di processi concorrenti.

### Sintassi e costrutti di base

#### Numbers

+ `B#val` used to store value in base **B**

+ `$char` used for ASCII values

#### Atoms

È possibile immaginarli come delle specie di Enum

#### Tuples

Sequenza di dati che inizia con le graffe

Le tuple sono usate per raggruppare un numero fisso di elementi. Gli elementi di una tuple possono essere di qualsiasi tipo.

```erlang
% Tuple con vari tipi di dati
Tuple = {123, "walter", cazzola}.
% Tuple vuota
EmptyTuple = {}.
Le tuple possono anche essere annidate.
```

```erlang
NestedTuple = {{1, 2}, 3}.
```
`==` operatore di matching

#### Lists

+ `|` append syntax to lists

+ `[B|L]` B è sempre un singolo elemento per definizione delle liste

> Le stringhe sono liste di char

Le liste sono strutture di dati che possono contenere un numero variabile di elementi. Possono essere dinamicamente modificate.

```erlang
% Lista vuota
EmptyList = [].

% Lista con elementi
List = [1, 2, 3].

% Lista costruita tramite cons (|) operator
ListWithCons = [1 | [2, 3]].
```

Le liste possono essere confrontate e manipolate con funzioni come length e append.

```erlang
% Lunghezza della lista
ListLength = length([1, 2, 3]).

% Concatenazione di liste
CombinedList = [1, 2] ++ [3, 4].
```

#### Assignment & Pattern Matching

Le "var" iniziano con la lettera maiuscola

Il pattern matching è una caratteristica fondamentale di Erlang che permette di associare variabili ai valori in modo efficiente e controllare il flusso di esecuzione.

```erlang
% Esempio di pattern matching su una tuple
{X, Y} = {10, 20}.
È possibile anche usare il pattern matching con liste.
```

```erlang
% Pattern matching per decomporre una lista
[Head | Tail] = [1, 2, 3].
```

#### Functions 

Erlang organizza il codice in moduli, che contengono funzioni. Una funzione può avere più clausole definite con pattern differenti.

```erlang
-module(example).
-export([double/1]).

% Funzione per raddoppiare un numero
double(X) -> X * 2.
```
Una funzione può anche utilizzare guardie per controllare ulteriori condizioni.


```erlang
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).
```
#### Map, Filter, reduce

```erlang
-module(mfr).
-export([map/2,filter/2,reduce/2]).
map(_, []) -> [];
map(F, [H|TL]) -> [F(H)|map(F,TL)].

filter(_, []) -> [];
filter(P, [H|TL]) -> filter(P(H), P, H, TL).
filter(true, P, H, L) -> [H|filter(P, L)];
filter(false, P, _, L) -> filter(P, L).

reduce(F, [H|TL]) -> reduce(F, H, TL).
reduce(_, Q, []) -> Q;
reduce(F, Q, [H|TL]) -> reduce(F, F(Q,H), TL).
```

#### List comprehension
```
[X||Qualifier1, ..., Qualifier_n]
```
Le comprensioni di liste in Erlang permettono di creare liste a partire da altre liste in maniera concisa.

```erlang
% Esempio di list comprehension
Doubled = [X * 2 || X <- [1, 2, 3, 4]].
```
È possibile anche filtrare elementi nelle comprensioni di liste.

```erlang
% Comprensione con filtro
EvenNumbers = [X || X <- [1, 2, 3, 4], X rem 2 == 0].
```

```erlang
-module(sort).
-export([qsort/2]).

qsort(_, []) -> [];
qsort(P, [Pivot|T]) ->
    qsort(P, [X || X <- T, P(X, Pivot)]) ++ [Pivot] ++ qsort(P, [X || X <- T, not P(X, Pivot)]).
```
# Simboli e concetti chiave
[[es1.ml]]
`let`: Per dichiarare variabili e funzioni.

`=`: Per l'assegnamento.

`;;`: Per terminare un'espressione.

`::`: Per aggiungere un elemento all'inizio di una lista.

`[]`: Per rappresentare una lista vuota.

`[a; b; c]` :  Per rappresentare una lista composta da a b e c

`fst`: Per accedere al primo elemento di una tupla.

`snd`: Per accedere al secondo elemento di una tupla.

`List.fold_left`: Per applicare una funzione a tutti gli elementi di una lista.

`List.hd` e `List.tl`: Per accedere al primo elemento e alla coda di una lista.

`List.sort`: Per ordinare una lista.

`List.merge`: Per unire due liste ordinate.


> <b>Record</b>: Un record è una struttura dati che contiene un insieme di campi con nomi e tipi associati. Nel nostro caso, il record temperatura ha due campi: valore (un numero a virgola mobile) e unita (una stringa).

> <b>Pattern matching</b>: Utilizzato implicitamente in molte funzioni per "scomporre" le liste e le tuple. è un meccanismo molto potente che permette di "scomporre" un dato complesso (come un record o una lista) e agire in base alla sua struttura. Nel codice, il pattern matching viene utilizzato per:
- Estrarre il valore e l'unità da una temperatura.
- Selezionare la formula di conversione appropriata in base alla scala.
- Iterare su una lista di scale.

> <b>Ricorsione</b>: La ricorsione è una tecnica di programmazione in cui una funzione si chiama direttamente o indirettamente. La funzione any2ct è ricorsiva perché si chiama se stessa per processare la coda della lista delle scale.

[[es2.ml]]
`type`: Definisce un nuovo tipo di dato, nel nostro caso temperatura.

`{ }`: Viene utilizzato per creare record, che sono simili a strutture in altri linguaggi, contenenti campi (come valore e unita).

`match with`: È un costrutto fondamentale in OCaml per eseguire il pattern matching, ovvero analizzare la struttura di un dato e eseguire azioni diverse in base ai casi.

`->`: Indica l'associazione tra un pattern e un'espressione nel pattern matching.

`.`: Operatore di accesso ai campi di un record.

`::`: Operatore di concatenazione di liste.

`List.rev`: Funzione che inverte l'ordine degli elementi di una lista.

`Format.fprintf`: Funzione per la formattazione dell'output.

`@` : per concatenare le liste
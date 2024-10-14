# Lez02.md

## linguaggio di programmazione funzionale
I linguaggi funzionali sono basati principalmente sulle funzioni, concepite come funzioni matematiche senza effetti collaterali (side-effects). In altre parole, ogni funzione riceve un input e restituisce un output senza modificare lo stato del programma.

### Caratteristiche principali:
- Ricorsione come struttura di controllo primaria (al posto dei loop).
- Stati immutabili, ossia le variabili non cambiano valore dopo essere state assegnate.
- No side-effects: le funzioni pure non modificano variabili globali o lo stato del programma.

## Perché scegliere un linguaggio funzionale
- Codice ottimizzato: Poiché si eliminano gli effetti collaterali, il codice è più facile da ottimizzare.
- Semplicità nel debugging: Grazie alla chiarezza e prevedibilità delle funzioni pure.
- Verifica formale: La natura matematica permette di dimostrare le proprietà del codice.

## Concetti fondamentali:
- Astrazione: Definire una funzione.
- Applicazione: Usare (o applicare) una funzione.
## λ-Expressions:
Le lambda expressions sono funzioni anonime che possono essere definite senza un nome, usate per rappresentare operazioni matematiche o logiche. In OCaml e altri linguaggi funzionali, permettono di definire e applicare funzioni in modo conciso, ad esempio: λx. x + 1 rappresenta una funzione che aggiunge 1 a un dato input 𝑥

Nel calcolo λ, le funzioni vengono definite in modo molto astratto. Ad esempio:

- λx.x+1 è una funzione che aggiunge 1 a 𝑥
- (λx.x+1)7 applica la funzione a 7 restituendo 8.

## OCaml e il paradigma funzionale
OCaml è una versione di ML con alcune funzionalità extra, che rende facile scrivere codice funzionale.

Esempio di una funzione:

```ocaml
let succ = fun x -> x + 1;;
```
Oppure in forma abbreviata:

```ocaml
let succ x = x + 1;;
```
## Scoping e funzioni di prima classe
Le funzioni in OCaml sono first-class citizens, cioè possono essere passate come argomenti e restituite come valori da altre funzioni.

Esempio di composizione di funzioni:
```ocaml
let compose f g x = f (g x);;
```
Questo crea una nuova funzione che applica 𝑔(𝑥) e poi 𝑓 al risultato di 𝑔(𝑥)

## Pattern Matching
Il pattern matching è un potente strumento in OCaml per gestire dati e flussi di controllo in modo chiaro, spesso preferito rispetto a costrutti condizionali come if-else.

Esempio:

```ocaml
let invert x = match x with
  | true -> false
  | false -> true ;;
  ```
## Funzioni ricorsive
Le funzioni ricorsive chiamano se stesse per risolvere problemi iterativi.

Esempio di fattoriale:

```ocaml
let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;
```
OCaml supporta anche la `tail recursion`, che ottimizza l'uso della memoria, evitando di creare molti frame di chiamata durante la ricorsione.

## Tipi di dati in OCaml
OCaml è fortemente e staticamente tipizzato, il che significa che i tipi vengono verificati a tempo di compilazione e non sono consentiti errori di tipo. Inoltre, OCaml effettua inferenza automatica dei tipi, quindi non è necessario specificare i tipi nella maggior parte dei casi.

### Collezioni:
- Liste: Sequenze di elementi dello stesso tipo, create con [] e manipolate con il costruttore ::.

```ocaml
let my_list = [1; 2; 3];;
```
Operazioni come concatenare due liste:

```ocaml
let l1 = [1; 2; 3];;
let l2 = [4; 5; 6];;
let l3 = l1 @ l2;;
```
- Tuples: Liste a lunghezza fissa e tipi eterogenei.

```ocaml
let my_tuple = (1, "hello", true);;
```
- Array: Simili alle liste, ma a lunghezza variabile e con accesso diretto tramite indice.

```ocaml
let my_array = [| 1; 2; 3 |];;
my_array.(0) <- 10;;
```
- Records: Strutture di dati con campi eterogenei, accessibili tramite nomi.

```ocaml
type person = { name : string; mutable age : int };;
let p = { name = "Walter"; age = 35 };;
```
- Varianti:
Un tipo variante definisce una serie di "casi" o valori che una variabile può assumere.

```ocaml
type int_option = Nothing | AnInteger of int ;;
```
Con il pattern matching, possiamo manipolare questi valori in modo molto elegante:

```ocaml
let invert_option = function
  | Nothing -> None
  | AnInteger x -> Some x ;;
  ```
## Operatori e stringhe
Le stringhe in OCaml sono immutabili e possono essere manipolate tramite moduli come String. Esistono operatori di concatenazione `(^)` e accesso posizionale `(.[n])`.

## Conclusione
Il paradigma funzionale, come implementato in OCaml, rende il codice più modulare e sicuro, grazie alla gestione di funzioni pure, tipizzazione forte e immutabilità. Utilizzare strumenti come la ricorsione, il pattern matching e le espressioni λ consente di scrivere programmi concisi e altamente ottimizzati​(02. datatypes in ml.4)​(02. datatypes in ml.4).

-----

```ocaml
  let compose f g x = f (g x );; 
  let compose (f, g) x = f (g x );; 
```


## Pattern Matching

```ocaml
  match expression with
    | pattern when boolean expression -> expression
    | pattern when boolean expression -> expression
  
```

```ocaml
  let invert x =
    match x with
      | true -> false
      | false -> true ;;
  let invert’ = function
    true -> false | false -> true ;;
```


### Recursion

> Allow tail recursion

```ocaml
  let rec fact(n) = if n<=1 then 1 else n*fact(n-1);
  let main() =
    print_endline("fact( 5) : - "^string_of_int(fact(5)));
    print_endline("fact( 7) : - "^string_of_int(fact(7)));
    print_endline("fact( 15) : - "^string_of_int(fact(15)));
    print_endline("the largest admissible integer is ;- "^string_of_int(max_int)); print_endline("fact( 25) : - "^string_of_int(fact(25)));;
  main();;
```
---

## String

^ = concatenation
.[] = positional access to char
s.[n] <- c = change nth char w/ c


---

### List

+ homogeneous
+ cons operator `::`
+ concatenation operator `@` (inefficient)




rlwrap

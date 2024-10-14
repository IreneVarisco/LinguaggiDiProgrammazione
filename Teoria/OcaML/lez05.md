# Lez05.md

## Currying

tecnica che permette di trasformare una funzione che accetta più argomenti in una serie di funzioni ognuna con un solo argomento.

```ocaml
let f x y = y/ .x ;;
let g = f 2. ;;
```
Nell'esempio sopra, abbiamo fissato il primo parametro (2.), creando una nuova funzione g che calcola il reciproco di un numero.

Questo approccio consente di effettuare applicazioni parziali, creando nuove funzioni da una base.

### Utilizzo Prametri Nominali
possiamo utilizzare parametri nominali per dare un nome ai parametri della funzione, il che consente una maggiore flessibilità durante l'applicazione parziale.

se voglio dare il nome al terzo argomento devo peffo darlo anche a quelli prima

```ocaml
let compose ~f ~g x = f (g x)
let compose’ = compose ~g: (fun x -> x**3.)
```

è utile quando sai che uno degli argomenti è fissato, oppure quando gli argomenti non li hai tutti a disposizione nello stesso momento


## map filter e reduce
non ho capito, sostituisci questa frase con il concetto di cui sta parlando e dimmi a cosa serve

### Map
applica una funzione a ciascun elemento di una lista e restituisce una nuova lista con i risultati.

rapresenta il pattern di programmazione che ricorre di più in programmazione funzionale
Esempio di map:

```ocaml
let double x = x * 2;;
List.map double [1; 2; 3; 4];;  (* Restituisce [2; 4; 6; 8] *)
```
implementazione possibile di map con `recall`:
```ocaml
let rec map f = function
h::l1 -> f h::map f l1
| _ -> [];;

```
funzioni utili: 
- `exists` restituisce vero se almeno un elemento che matcha il predicato
- `forall` restituisce vero quando tutti glli elementi matchano il predicato

### Filter
per filtrare alcuni elementi dalla lista a seconda del filtro(condizione booleana)

```ocaml
let rec filter p = function
[] -> []
| h::l -> if p h then h :: filter p l else filter p l
```

```cmd
# filter (fun x -> x < 7) l ;;
- : int list = [1; 2; 3; 4]
```

altro esempio:
```ocaml
let is_even x = x mod 2 = 0;;
List.filter is_even [1; 2; 3; 4];;  (* Restituisce [2; 4] *)
```

### Reduce o Folding
 riduce tutta la lista ad un singolo valore attraverso una funzione cumulativa

 ```ocaml
let rec reduce acc op = function
[] -> acc
| h::tl -> reduce (op acc h) op tl ;;
```

 si utilizza per applicare una funzione binaria a tutti gli elementi di una lista.

funzioni utili:
- `exists` restituisce vero se almeno un elemento che matcha il predicato
- `forall` restituisce vero quando tutti glli elementi matchano il predicato
le funzioni possono essere associative in 2 modi
 - `right fold`: combinando il primo elemento coi risultati del ricorsivamente combinando il resto
 - `left fold`: combinando i risultati del ricorsivamente combinando tutto a parte l'ultimo elemento, con l'ultimo elemento

`List` fornisce le funzioni `fold_left` e `fold_right`

Esempio di fold_left:

```ocaml
let sum = List.fold_left (+) 0 [1; 2; 3; 4];;  (* Restituisce 10 *)
```
Esempio di fold_right:

```ocaml
let concatenate = List.fold_right (^) ["a"; "b"; "c"] "";;  (* Restituisce "abc" *)
```

## Combinazioni di Map, Filter e Reduce
È possibile combinare queste funzioni per creare operazioni più complesse. Ad esempio, possiamo usare map e reduce per verificare se almeno un elemento di una lista soddisfa un predicato:

Esempio di combinazione:

```ocaml
let exists p l = List.fold_left (||) false (List.map p l);;
exists (fun x -> x mod 2 = 0) [1; 3; 5; 6];;  (* Restituisce true, perché 6 è pari *)
```

## Miscellaneous - Iterare sulle liste
### zip
lo zipping permette di combinare due liste accoppiando i loro elementi. L'operazione termina quando una delle due liste è vuota.

è equivalente a `List.assoc`

Esempi:
```ocaml
let rec zip_longest l1 l2 =
match (l1, l2) with
([],[]) | (_, []) | ([], _) -> []
| (h1::l1’, h2::l2’) -> (h1,h2)::(zip_longest l1’ l2’) ;;
```

```ocaml
let rec zip l1 l2 = match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (h1::t1, h2::t2) -> (h1, h2) :: zip t1 t2;;
zip [1; 2; 3] ['a'; 'b'; 'c'];;  (* Restituisce [(1, 'a'); (2, 'b'); (3, 'c')] *)
```

### Group by

per riconoscere una lista a secondo di una proprietà numerica

```ocaml
type ’a group = { mutable g: ’a list } ;;
let empty_group = function x -> { g = [] } ;;
let rec group_by l ?(ris:’a group array = (Array.init 10 empty_group)) f =
match l with
[] -> ris
| h::l1 ->
( ris.((f h)).g <- ris.((f h)).g@[h] ;
group_by l1 ~ris:ris f );;
```

## Advance on Function
funzioni con un numero variabile di argomenti

```ocaml
let arg x = fun y rest -> rest (op x y) ;;
let stop x = x;;
let f g = g init;;
```

#### pairwise
La funzione pairwise permette di creare una lista di coppie adiacenti a partire da una lista.

Esempio:

```ocaml
let rec pairwise = function
  | h1::h2::t -> (h1, h2) :: pairwise (h2::t)
  | _ -> [];;
pairwise [1; 2; 3; 4];;
(* Restituisce [(1, 2); (2, 3); (3, 4)] *)
```
#### Enumerate
 Questa funzione associa a ogni elemento della lista un indice (contatore) partendo da 0.

Esempio:

```ocaml
let enumerate l =
  let rec aux acc n = function
    | [] -> List.rev acc
    | h::t -> aux ((n, h)::acc) (n+1) t
  in aux [] 0 l;;

enumerate ["a"; "b"; "c"];;  (* Restituisce [(0, 'a'); (1, 'b'); (2, 'c')] *)
```
### Functions with a Variable Number of Arguments
#### arg
 permette di costruire una funzione che accumula i valori passati, fino a che non viene chiamata una funzione di arresto (`stop`).

Esempio:

```ocaml
let op = fun x y -> x + y;;
let init = 0;;

let arg x = fun y rest -> rest (op x y);;
let stop x = x;;

let f g = g init;;
f (arg 1) (arg 2) (arg 3) stop;;  (* Restituisce 6 *)
```
Questa struttura permette di accumulare i valori in modo flessibile e iterativo.

Questa struttura permette di definire funzioni che accettano un numero variabile di argomenti e le combina insieme tramite `op`.

l'approccio precedente ha necessità di essere ricaricata ogni volta in cui hai bisogno di un tipo diverso per f, quindi rimuovere l'istanza precedente

per risolverlo dobbiamo usare 
- un tipo di dato astratto (OptVarADT)

```ocaml
module type OpVarADT =
sig
type a and b and c
val op: a -> b -> c
val init : c
end
```

- un funtore (VarArgs): consente di creare funzioni che operano su tipi specifici definiti all'interno di un'astrazione ADT (Abstract Data Type).
```ocaml
module VarArgs (OP : OpVarADT.OpVarADT) =
struct
let arg x = fun y rest -> rest (OP.op x y) ;;
let stop x = x;;
let f g = g OP.init;;
end
```
- alcune implementazioni concrete per ADT(Implementazione con Concatenazione di Stringhe)
```ocaml
module Sum = struct
type a = int and b = int and c = int
let op = fun x y -> x + y ;;
let init = 0 ;;
end
```
```ocaml
module StringConcat = struct
type a=string and b=string list and c=string list
let op = fun (x: string) y -> y @ [x] ;;
let init = [] ;;
end
```

### instanziare OpVarADT con una lista generica
necessito di un funtore 
quando c'è un problema astrarre è sempre la soluzione
```ocaml
module ListConcat = struct
type a and b = a list and c = a list
let op = fun (x: a) y -> y @ [x] ;;
let init = [] ;;
end```

se non si può usare un tipo parametrizzato   puoi usare una lingua modulo per aggiungere parametrizazzione , creando il modulo  List Concat un funt
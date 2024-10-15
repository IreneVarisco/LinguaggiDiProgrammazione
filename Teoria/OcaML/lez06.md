# Lez06.md

## DFS

Dato un grafo e un nodo iniziale, il DFS esplora il grafo partendo dal nodo dato. Questo approccio costruisce un albero. Ci servono quindi il tipo albero (tree) e il grafo.

```ocaml
type ’a tree = Leaf of ’a | Tree of (’a * ’a tree list);;
```

Vogliamo avere una definizione matematica del grafo:

- Insieme dei nodi (lista dei nodi)
- Insieme degli archi (lista di tuple di 2 nodi)

In termini funzionali &rarr; matematici

Creiamo un interfaccia per lavorare con grafi generici in OCaml


Definiamo un tipo polimorfico 'a graph, dove 'a è un `generico` che rappresenta il tipo dei nodi nel grafo. Ciò significa che il grafo può contenere nodi di qualsiasi tipo.

```ocaml
  module type GraphADT =
  sig
    type ’a graph
    val empty : unit -> ’a graph
    val add_node : ’a -> ’a graph -> ’a graph
    val add_arc : ’a -> ’a -> ’a graph -> ’a graph val adjacents : ’a -> ’a graph -> ’a list
    val node_is_in_graph : ’a -> ’a graph -> bool val is_empty : ’a graph -> bool
    exception TheGraphIsEmpty
    exception TheNodeIsNotInGraph
  end;;
```

- `empty` : Questa funzione crea un nuovo grafo vuoto.
- `add_node` : Aggiunge un nuovo nodo al grafo.
- `add_arc` : Aggiunge un nuovo arco tra due nodi del grafo.
- `adjacents` : Restituisce una lista di nodi adiacenti a un nodo dato.
- `node_is_in_graph` : Verifica se un nodo è presente nel grafo.
- `is_empty` : Verifica se il grafo è vuoto.

```ocaml
  module Graph : GraphADT =
    struct
      type ’a graph = Graph of ( ’a list ) * ( ( ’a * ’a ) list )

  (*funzione che crea un grafo vuoto*)
      let empty() = Graph([], []) let is_empty = function
        Graph(nodes, _) -> (nodes = []) exception TheGraphIsEmpty
      exception TheNodeIsNotInGraph

  (* controlla se l'elemento appartiene alla lista *)
      let rec is_in_list ?(res=false) x = function [] -> res
        | h::tl -> is_in_list ~res: (res || (x=h)) x tl

  (* controlla se l'elemento è nel grafo*)
        let node_is_in_graph n = function Graph(nodes, _) -> is_in_list n nodes

  (* aggiunge un elemento a una lista se non è ancora presente *)
      let rec add_in_list ?(res=[]) x = function
        [] -> List.rev x::res
        | h::tl when (h=x) -> List.rev_append tl (h::res)
        | h::tl -> add_in_list ~res: (h::res) x tl

  (* operazioni per aggiungere nuovi nodi e archi(con i loro nodi) al grafo rispettivamnete *)
      let add_node n = function
        Graph( [], [] ) -> Graph( [n], [] )
        | Graph( nodes, arcs ) -> Graph( (add_in_list n nodes), arcs )
      let add_arc s d = function
        Graph(nodes, arcs) ->
          Graph( (add_in_list d (add_in_list s nodes)), (add_in_list (s,d) arcs) )

  (* restituisce il nodo adiacente al nodo in input *)
      let adjacents n =
        let adjacents n l = List.map snd (List.filter (fun x -> ((fst x) = n)) l)
        (*filtro tutte quelle che come primo elemento dell'arco non hanno x e sui rimanenti controllo se il nodo sorgente è il nodo che stiamo analizzando*)
      in function
        Graph(_, arcs) -> adjacents n arcs
        (*stiamo utilizando una mappa mi tengo la lista dei secondi nodi*)

    end;;

```
trasformiamo la lista di archi in un grafo
```ocaml
open Graph

let arcs_to_graph arcs =
  let rec arcs_to_graph g = function
    [] -> g
  | (s,d)::tl -> arcs_to_graph (add_arc s d g) tl
  in arcs_to_graph (empty()) arcs
(* extract a tree out of acyclic graph with the given node as the root *)
  let graph_to_tree g root =
    let rec make_tree n = function
      [] -> Leaf(n)
    | adj_to_n -> Tree(n, (make_forest adj_to_n))
(* prende il valore del nodo e controlla se è una foglia, altrimeni costruisce un altro albero *)
    and make_forest = function
      [] -> []
    | hd::tl -> (make_tree hd (adjacents hd g))::(make_forest tl)
    in make_tree root (adjacents root g)
(*  restituisce una lista di alberi con radice e adiacenti *)
```
spiegazione:
- make_tree: Costruisce un albero a partire da un nodo radice. Se non ci sono nodi adiacenti, crea una foglia. Altrimenti, costruisce un nodo interno con il nodo radice come valore e una lista di alberi figli costruiti ricorsivamente.
- make_forest: Costruisce una foresta (lista di alberi) a partire da una lista di nodi. Per ogni nodo nella lista, crea un albero utilizzando make_tree con il nodo come radice e i suoi nodi adiacenti, aggiungendo l'albero alla foresta.

La funzione principale `graph_to_tree` chiama `make_tree` con il nodo radice e la lista dei suoi nodi adiacenti.

Notare come make tree e make forest si chiamano a vicenda e sono definite contemporaneamente.


ora specifichiamo la dfs:
```ocaml
open Graph

let dfs g v =
(*esplora un grafo andando il più in profondità possibile lungo un percorso prima di tornare indietro.*)

  let rec dfs g v g' = function
    [] -> g'
  | hd::tl when (node_is_in_graph hd g') -> dfs g v g' tl
  | hd::tl -> dfs g v (add_arc v hd (dfs g hd (add_node hd g') (adjacents hd g))) tl
  (* g' è il grafo finale e rappresenta l'albero di ricerca *)
  in
    if (is_empty g) then raise TheGraphIsEmpty
    else if not (node_is_in_graph v g) then raise TheNodeIsNotInGraph

    (* converte il grafo risultante in un albero*)
      else graph_to_tree (dfs g v (add_node v (empty())) (adjacents v g)) v
```
Spiegazione della funzione dfs:
- Esplora ogni nodo adiacente a v: 
  + Se è già stato visitato (presente nel grafo g'), lo ignora.
  + Altrimenti, aggiunge un arco da v al nodo adiacente, visita ricorsivamente il nodo adiacente, e aggiunge il risultato al grafo g'.
---

`= function` &rarr; syntactic sugar for `= match x with ...`

# Lez06.md

---

In termini funzionali &rarr; matematici

`= function` &rarr; syntactic sugar for `= match x with ...`


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

```ocaml
  module Graph : GraphADT =
    struct
      type ’a graph = Graph of ( ’a list ) * ( ( ’a * ’a ) list )
      let empty() = Graph([], []) let is_empty = function
        Graph(nodes, _) -> (nodes = []) exception TheGraphIsEmpty
      exception TheNodeIsNotInGraph
      (* checks if an element belongs to the list *)
      let rec is_in_list ?(res=false) x = function [] -> res
      | h::tl -> is_in_list ~res: (res || (x=h)) x tl
      (* checks if a node is in the graph *)
      let node_is_in_graph n = function Graph(nodes, _) -> is_in_list n nodes
      (* ... *)
    end;;

  ```

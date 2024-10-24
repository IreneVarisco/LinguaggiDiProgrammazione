type individual = {name: string; surname: string; age: int};;
(* nodi *)

let interdependency = ["friendship"; "kinship"; "financial exchange"; "dislike"; "sexual relationships"; "beliefs"; "knowledge or prestige"];;
(* tipi di archi *)

module type makeGraph =
sig
  type 'a graph
  val empty : unit -> 'a graph
  val add_node : 'a -> 'a graph -> 'a graph
  val add_arc : 'a -> 'a -> 'a graph -> 'a graph
  val adjacents : 'a -> 'a graph -> 'a list
  val node_is_in_graph : 'a -> 'a graph -> bool
  val is_empty : 'a graph -> bool
  exception TheGraphIsEmpty
  exception TheNodeIsNotInGraph
end;;

module SocialNetwork : makeGraph =
struct
  type 'a graph = SocialNetwork of ('a list) * (('a * 'a) list)

  let empty () = SocialNetwork ([], [])

  let is_empty = function
    | SocialNetwork (nodes, _) -> nodes = []

  exception TheGraphIsEmpty
  exception TheNodeIsNotInGraph

  let rec is_in_list ?(res = false) x = function
    | [] -> res
    | h :: tl -> is_in_list ~res:(res || (h = x)) x tl

  let node_is_in_graph n = function
    | SocialNetwork (nodes, _) -> is_in_list n nodes

  let rec add_in_list ?(res = []) x = function
    | [] -> List.rev (x :: res)
    | h :: tl when h = x -> List.rev_append res (h :: tl)
    | h :: tl -> add_in_list ~res:(h :: res) x tl

  let add_node n = function
    | SocialNetwork ([], []) -> SocialNetwork ([n], [])
    | SocialNetwork (nodes, arcs) -> SocialNetwork (add_in_list n nodes, arcs)

  let add_arc s d = function
    | SocialNetwork (nodes, arcs) ->
        SocialNetwork (add_in_list d (add_in_list s nodes), add_in_list (s, d) arcs)

  let adjacents n = function
    | SocialNetwork (_, arcs) -> List.map snd (List.filter (fun (s, _) -> s = n) arcs)
end;;

open SocialNetwork

let arcs_to_graph arcs =
  let rec arcs_to_graph g = function
    | [] -> g
    | (s, d) :: tl -> arcs_to_graph (add_arc s d g) tl
  in
  arcs_to_graph (empty ()) arcs

let dfs s graph =
  let rec dfs_aux to_visit visited =
    match to_visit with
    | [] -> visited
    | node :: tl ->
        if List.mem node visited then
          dfs_aux tl visited
        else
          let adj = adjacents node graph in
          dfs_aux (adj @ tl) (node :: visited)
  in
  dfs_aux [s] []

(* Example usage *)
let () =
  let g = empty () in
  let g = add_node {name = "Alice"; surname = "Smith"; age = 30} g in
  let g = add_node {name = "Bob"; surname = "Johnson"; age = 25} g in
  let g = add_node {name = "Charlie"; surname = "Brown"; age = 20} g in
  let g = add_arc {name = "Alice"; surname = "Smith"; age = 30} {name = "Bob"; surname = "Johnson"; age = 25} g in
  let g = add_arc {name = "Bob"; surname = "Johnson"; age = 25} {name = "Charlie"; surname = "Brown"; age = 20} g in
  let visited = dfs {name = "Alice"; surname = "Smith"; age = 30} g in
  List.iter (fun ind -> Printf.printf "%s %s\n" ind.name ind.surname) visited

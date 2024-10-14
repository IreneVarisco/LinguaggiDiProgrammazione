(* Social Networks *)

(* A social network is a social structure made of individuals (or organizations)
 called nodes, which are tied (connected) by one or more specific types of
 interdependency, such as friendship, kinship, financial exchange, dislike,
 sexual relationships, or relationships of beliefs, knowledge or prestige.

 A graph is an abstract representation of a set of objects where some pairs of
 the objects are connected by links. The interconnected objects are represented
 by mathematical abstractions called vertices, and the links that connect some
 pairs of vertices are called edges.

 The exercise consists of:

   implementing the social network as a graph, i.e., to define the graph data
   structure with the operations you consider necessary to implement a social
   network implementing an operation that visits in a convenient way all the 
   elements of the graph testing it against a dummy social network.
*)

module type Graph = 
  sig
    type 'a graph_term = { nodes : 'a list; connection_type: string;  edges : ('a * 'a) list }
  end;;


(*
  GROUP

  Test-Example
  S = {True, False}, add = or, i = False
  S = Z, add = +, i = 0
  S = Z4 = {0, 1, 2, 3}, add = * where a*b=(a*b)%4, i = 1
  S = {a}*, add: {a}*×{a}* --> {a}* where x1·x2 is x1 if #x1 > #x2, x2 otherwise, i = the empty string
*)

module type Group = sig
  type t
  val empty : t
  val ( <+> ) : t -> t -> t
end

module Make (M : Group) = struct
  let fold ms = 
    M.(List.fold_left (<+>) empty ms)
end

module Sum = struct
  type t = int
  let empty = 0
  let (<+>) = (+)
end

module Prod = struct
  type t = int
  let empty = 1
  let (<+>) = ( * )
end

module Any = struct
  type t = bool
  let empty = false
  let (<+>) = (||)
end

module All = struct
  type t = bool
  let empty = true
  let (<+>) = (&&)
end

module First (T : sig type t end) = struct
  type t = T.t option
  let empty = None
  let (<+>) m1 m2 =
    match m1,m2 with
      | Some x,Some _ -> Some x
      | Some x,None -> Some x
      | None,Some x -> Some x
      | None,None -> None
end

module Last (T : sig type t end) = struct
  type t = T.t option
  let empty = None
  let (<+>) m1 m2 =
    match m1,m2 with
      | Some _,Some x -> Some x
      | Some x,None -> Some x
      | None,Some x -> Some x
      | None,None -> None
end

module List (T : sig type t end) = struct
  type t = T.t list
  let empty = []
  let (<+>) = List.append
end



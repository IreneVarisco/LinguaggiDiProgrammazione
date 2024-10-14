(*
  MONOID


  Test-Example
  S = {True, False} add = or i = False 
  S = Zn={0,1,...,n-1} add = + where a+b=(a+b)%n i = 0
*)

module type Monoid = sig
  type t
  val empty : t
  val ( <+> ) : t -> t -> t
end

module Make (M : Monoid) = struct
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

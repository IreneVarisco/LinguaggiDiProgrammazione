(* Factorial *)
let rec fact = function
  0 -> 1
  | n -> n * fact (n - 1)
;;



(* Tail-recursive Factorial *)
let fact n =
  let rec fact acc = function
    0 -> acc
  | n -> fact (n * acc) (n - 1)
  in fact 1 n;;

let lista = [6;6;6;6;6;5;5;5;2;9;9;1;9;9];;

let count lista =
  let rec count value ls = function
      [] -> List.rev(ls) 
    | h::h2::tl when h = h2 -> count (value + 1) ls (h2::tl)
    | _::tl -> count 1 (value::ls) tl
  in count 1 [] lista;;



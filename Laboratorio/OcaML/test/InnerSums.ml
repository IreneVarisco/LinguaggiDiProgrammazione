let lista = [1;2;3;0;2;2;0;0;0;1;1;0];; 

let innersum lista =
  let rec innersum value ls = function
      [] -> List.rev(ls) 
    | h::0::tl when value <> 0 -> innersum 0 (h + value::ls) tl
    | h::tl -> innersum (h + value) ls tl
  in innersum 0 [] lista;;

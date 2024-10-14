let contiguous_regions lst =
  let rec contiguous_regions cnt lst = function
      [] -> List.rev(lst) 
    | h::h2::tl when h2 > h -> contiguous_regions (cnt + 1) lst (h2::tl)
    | _::tl -> contiguous_regions 1 (cnt::lst) tl
  in contiguous_regions 1 [] lst;;

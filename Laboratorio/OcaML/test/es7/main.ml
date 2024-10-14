let main() =
  let s = VariantStack.VariantStack.(empty |> push 10 |> push 20 |> pop |> top)
  in
  print_endline (string_of_int s);;

  main();;

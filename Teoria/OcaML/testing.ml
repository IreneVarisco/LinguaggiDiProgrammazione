
  let rec fact(n) = if n<=1 then 1 else n*fact(n-1);
  let main() =
    print_endline("fact( 5) : - "^string_of_int(fact(5)));
    print_endline("fact( 7) : - "^string_of_int(fact(7)));
    print_endline("fact( 15) : - "^string_of_int(fact(15)));
    print_endline("the largest admissible integer is ;- "^string_of_int(max_int)); print_endline("fact( 25) : - "^string_of_int(fact(25)));;
  main();;

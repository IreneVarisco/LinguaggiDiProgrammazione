let lst_num = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let lst_char = ['a';'b'; 'c'; 'd'; 'e'; 'f'; 'g']

let rec zip_longest ls1 ls2 = function
    ([], []) | (_, []) | ([], []) -> []
  | (h1 :: l1', h2::l2) -> (h1, h2) :: (zip_longest l1' l2') ;;

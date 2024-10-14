(* Put into a list, called alkaline_earth_metals, the atomic numbers of the six
   alkaline earth metals: beryllium (4), magnesium (12), calcium (20),
   strontium (38), barium (56), and radium (88). Then *)

let alkaline_earth_metals =
  ("beryllium", 4)::("magnesium", 12)::("calcium", 20)::("strontium", 38)::("barium", 56)::("radium", 88)::[];;
(* let alkaline_earth_metals = [4; 12; 20; 38; 56; 88];; *)


(* Write a function that returns the highest atomic number in
   alkaline_earth_metals. *)
let (>:) a b = (snd a) - (snd b);;
let max a b = if (a >: b >= 0) then a else b;;
let heaviest lst =
  List.fold_left max (List.hd lst) (List.tl lst) ;;


(* Same function but without syntactic sugar *)
let rec high' max = function
  | [] -> max
  | h::tl -> if (h >= max) then high' h tl else high' 0 tl



(* Write a function that sorts alkaline_earth_metals in ascending order (from
   the lightest to the heaviest). *)
(* let rec sort l =  *)
(*   match l with *)
(*   | [] -> [] *)
(*   | h::t -> insert h (sort t) *)
(* and insert e l =  *)
(*   match l with *)
(*   | [] -> [e] *)
(*   | h::t -> if e <= h then e :: l else h :: insert e t;; *)
let sort_ascending lst = List.sort (>:) lst;;

sort_ascending alkaline_earth_metals;;

(* Put into a second list, called noble_gases, the noble gases: helium (2),
   neon (10), argon (18), krypton (36), xenon (54), and radon (86). Then *)
(* let noble_gases = [2; 10; 18; 36; 54; 86];; *)
let noble_gases =
  [("helium", 2);("neon", 10);("argon", 18);("krypton", 36);("xenon", 54);("radon", 86)];;

type gases = { name : string; value : int}

(* Write a function (or a group of functions) that merges the two lists and
   print the result as couples (name, atomic number) sorted in ascending order
   on the element names. *)



(* merge lists alkaline_earth_metals and noble_gases w/ name and atmomic number
   as couple (name, atomic number) *)
let merge_elements metals gases =
  let sorted_metals = sort_ascending metals and
      sorted_gases = sort_ascending gases in
  List.merge (>:) sorted_metals sorted_gases;; 

merge_elements alkaline_earth_metals,
    

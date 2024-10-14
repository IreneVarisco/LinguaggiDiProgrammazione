module VariantStack : StackADT.StackADT = struct
  type 'a stack =
    | Empty
    | Some of 'a * 'a stack

  exception EmptyStack
  let empty = Empty
  let push =
    function
       Empty -> raise Empty
     | Some (_, s) -> s
  let top =
    function
        Empty -> raise EmptyStack
      | Some (elt, _) -> elt
end

module List.Stack : StackADT.StackADT = struct
  type 'a stack = 'a list
  let empty = []
  let push elt s = elt :: s

  let pop = 
    function
      [] -> raise EmptyStack
      | _ :: t -> t
   let top =
     function
       [] -> raise EmptyStack
      | h :: _ -> h
end

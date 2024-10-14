module type StackADT = sig
  type 'a stack
  exception EmptyStack

  val empty : 'a stack
  val push  : 'a -> 'a stack -> 'a stack
  val pop : 'a stack -> 'a stack
  val top : 'a stack -> 'a
end


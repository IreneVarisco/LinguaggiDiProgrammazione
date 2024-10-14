module type SommaInterfaccia = 
  sig
    type type_adder
    val adder : type_adder -> type_adder -> type_adder
  end;;

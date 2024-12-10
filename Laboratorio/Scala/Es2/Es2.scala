//squared_numbers that removes all non-numbers from a polymorphic list and returns the resulting list of squared numbers, e.g., squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil) should return List(1, 10000, 9.8596, List(100), (25,49)). Note that it recursively applies to substructures.
//intersect that given two generic lists, returns a new list that is the intersection of the two lists (e.g., intersect(List(1,2,3,4,5), List(4,5,6,7,8)) should return List(4,5)).
//symmetric_difference that given two lists, returns a new list that is the symmetric difference of the two lists. For example symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)) should return List(1,2,3,6,7,8).

object Es2 {
    def squared_numbers (l : List[Any]): List[Any] = {
        l.flatMap {
            case i: Int => List(i * i)
            case d: Double => List(d * d)
            case f: Float => List(f * f)
            case l: List[_] => squared_numbers(l)
            case _ => None
        }
    }

    def intersect(l1 : List[Any], l2 : List[Any]): List[Any] = {
        //l1.foreach{case l2.contains(_) => intersect += _}
        for(el <- l1 if l2.contains(el)) yield el
    }

    def symmetric_difference(l1 : List[Any], l2 : List[Any]): List[Any] = {
        (for(el <- l1 if !l2.contains(el)) yield el) ++ (for(el <- l2 if !l1.contains(el)) yield el)
        //l1.filterNot(l2.contains) ++ l2.filterNot(l1.contains)
    }

    def main (args :Array[String]): Unit = {
        val l1 = List(1, 2, 3, "formaggio", 2.0, "woo")
        val l2 = List(3, 4, "woo", 34, 1)

        println("quadrato:")
        println(squared_numbers(l1))

        println("\nintersezione:")
        println(intersect(l1,l2))

        println("\ndifferenza simmetrica")
        println(symmetric_difference(l1, l2))
    }
}
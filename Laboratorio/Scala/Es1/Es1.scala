object Es1 {
    def isPalindrome(s: String): Boolean = {
        val p = s.replaceAll("\\s", "")
        // p.toLowerCase == p.toLowerCase.reverse
        rev(p.toLowerCase) == p.toLowerCase
    }

    def rev(s: String): String = {
        var rev = ""
        //for (i <- s.length - 1 to 0 by -1) {
        //    rev += s(i)
        //} 
        rev = (for (i <- s.length - 1 to 0 by -1) yield s(i)).mkString

        rev
    }

    def isAnagram(s:String , Dictonary:List[String]): Boolean = {
        val a = s.replaceAll("\\s", "")
        Dictonary.exists(_.toLowerCase.sorted.equals(a.toLowerCase.sorted))
    }

    //Un numero perfetto è un numero intero positivo che è uguale alla somma di tutti i suoi divisori propri (escluso se stesso).
    def isPerfect(n: Int): Boolean = {
        (1 to n/2).filter(n % _ == 0).sum == n
    }

    def main(args: Array[String]): Unit = {
        val Dictonary = List("ciao", "mamma", "formaggio", "pippo", "pluto")
        val p = "i topi non avevano nipoti"
        val a = "mafroigog"
        val n = 28
        val p1 = "i topi non avevano nippoti"
        val a1 = "mafrhdoigog"
        val n1 = 29

        println("should be true:")
        println("palindrome: "+ isPalindrome(p))
        println("anagram: " + isAnagram(a, Dictonary))
        println("perfect: " + isPerfect(n))

        println("\nshould be false:")
        println("palindrome: "+ isPalindrome(p1))
        println("anagram: " + isAnagram(a1, Dictonary))
        println("perfect: " + isPerfect(n1))
    }
}
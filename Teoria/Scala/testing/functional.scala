import scala.runtime.LazyInt
class FunctionalScala {
  val is_palindrome = (s: String) => {
    val s1 = s.filterNot(x => List('.', ',', ';', '?', ' ').contains(x)).toLowerCase()
    s1.equals(s1.reverse)
  }

  def factors(number: BigInt, start: BigInt=2, list: List[BigInt]=Nil): List[BigInt] = {
    LazyList
      .iterate(start)(i => i + 1)
      .takeWhile(n => n <= number)
      .find(n => number % n == 0)
      .map(n => factors(number/n, n, list :+ n))
      .getOrElse(list)
  }
}

object FunctionalScala {
  def main(args: Array[String]) = {
    val fs = new FunctionalScala()

    // List("detartrated", "Do geese see God?", "Rise to vote, sir.")
    //   .map(x => f"[is_palindrome] $x : - ${fs.is_palindrome(x)}\n")
    //   .foreach(print(_))

    List(25, 400, 1970, 42, 32523, 7, 534587)
      .map(x => f"[factors] $x : - ${fs.factors(x)}\n")
      .foreach(print(_))
  }
}


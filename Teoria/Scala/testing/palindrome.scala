class FunctionalScala {
  val is_palindrome = (s: String) => {
    val s1 = s.filterNot(x => List('.', ',', ';', '?', ' ').contains(x))
  }
}

object FunctionalScala {
  def main(args: Array[String]) = {
    val fs = new FunctionalScala()
  }
}

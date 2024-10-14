class Anagram {
  def is_an_anagram(str: String, dict: List[String]): Boolean = {
    val sorted_str = str.sorted
    dict.map(_.sorted) contains sorted_str
  }
}

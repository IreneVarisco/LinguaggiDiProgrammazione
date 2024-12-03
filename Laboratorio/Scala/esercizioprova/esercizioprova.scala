case class Libro(
  Titolo: String, 
  Autore: String, 
  Anno: Int, 
  Genere: String
)

object esercizioprova {
    def main(args: Array[String]): Unit = {

        val Biblioteca = List(
            Libro("Il nome della rosa", "Umberto Eco", 1980, "Giallo storico"),
            Libro("1984", "George Orwell", 1949, "Distopia"),
            Libro("Neuromante", "William Gibson", 1984, "Fantascienza"),
            Libro("Cronache di Narnia", "C.S. Lewis", 1950, "Fantasy"),
            Libro("Fahrenheit 451", "Ray Bradbury", 1953, "Distopia"),
            Libro("Dune", "Frank Herbert", 1965, "Fantascienza"),
            Libro("Lo Hobbit", "J.R.R. Tolkien", 1937, "Fantasy"),
            Libro("Sulla strada", "Jack Kerouac", 1957, "Romanzo"),
            Libro("Delitto e Castigo", "Fëdor Dostoevskij", 1866, "Romanzo"),
            Libro("Mondo Nuovo", "Aldous Huxley", 1932, "Distopia")
        )

        val libriPerGenere = Biblioteca.groupBy(_.Genere)
        val conteggioLibri = Biblioteca.groupBy(_.Genere).map { case (genere, libri) => (genere, libri.length) }
        val libriPerAutore = Biblioteca.groupBy(_.Autore)

        println("elenco libri usciti dopo il 1900")
        controlloAnno(Biblioteca)
        println("\n libro più vecchio:" + oldest(Biblioteca))
        println("\n libri per genere: \n" + libriPerGenere.mkString("\n"))
        println("\n numero libri: \n" + conteggioLibri.mkString("\n"))
        println("\n libri per autore:")
        stampaPerAutore(libriPerAutore)
    }

    def controlloAnno(Biblioteca:List[Libro])={
        var libriAnno = Biblioteca.filter(_.Anno >= 1900)
        libriAnno.foreach(println)
    }

    def oldest(Biblioteca: List[Libro]): Libro = {
        Biblioteca.reduceLeft((a, b) => if (a.Anno < b.Anno) a else b)
    }

    def stampaPerAutore(libriPerAutore:Map[String, List[Libro]]) ={
        libriPerAutore.foreach{case (autore, libri) => println(s"Autore: $autore\nLibri: ${libri.mkString("\n")}\n")}
    }
}

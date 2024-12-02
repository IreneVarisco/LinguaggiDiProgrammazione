case class Libro(
  Titolo: String, 
  Autore: String, 
  Anno: Int, 
  Genere: String
)

object esercizioprova {
    def main(args: Array[String]): Unit = {
    val L1 = Libro("Il nome della rosa", "Umberto Eco", 1980, "Giallo storico")
    val L2 = Libro("1984", "George Orwell", 1949, "Distopia")
    val L3 = Libro("Neuromante", "William Gibson", 1984, "Fantascienza")
    val L4 = Libro("Cronache di Narnia", "C.S. Lewis", 1950, "Fantasy")
    val L5 = Libro("Fahrenheit 451", "Ray Bradbury", 1953, "Distopia")
    val L6 = Libro("Dune", "Frank Herbert", 1965, "Fantascienza")
    val L7 = Libro("Lo Hobbit", "J.R.R. Tolkien", 1937, "Fantasy")
    val L8 = Libro("Sulla strada", "Jack Kerouac", 1957, "Romanzo")
    val L9 = Libro("Delitto e Castigo", "Fëdor Dostoevskij", 1866, "Romanzo")
    val L10 = Libro("Mondo Nuovo", "Aldous Huxley", 1932, "Distopia")

    val Biblioteca = creaBiblioteca(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10)
    val libriPerGenere = Biblioteca.groupBy(_.Genere)
    val conteggioLibri = Biblioteca.groupBy(_.Genere).map { case (genere, libri) => (genere, libri.length) }
    val libriPerAutore = Biblioteca.groupBy(_.Autore)

    println("elenco libri usciti dopo il 1900")
    println(cercaLibro(Biblioteca, 1900))
    println("\n libro più vecchio:" + oldest(Biblioteca))
    println("\n libri per genere: \n" + libriPerGenere.mkString("\n"))
    println("\n numero libri: \n" + conteggioLibri.mkString("\n"))
    println("\n libri per autore:")
    stampaPerAutore(libriPerAutore)
}

def creaBiblioteca(L1:Libro, L2:Libro, L3:Libro, L4:Libro, L5:Libro, L6: Libro, L7:Libro, L8: Libro, L9: Libro, L10:Libro):List[Libro]={
    val Biblioteca = List(L1, L2, L3, L4, L5, L6, L7, L8, L9, L10)
    return Biblioteca
}

def cercaLibro(Biblioteca:List[Libro]):String = {
    return controlloAnno(Biblioteca).mkString("\n")
}

def controlloAnno(Biblioteca:List[Libro]):List[Libro]={
    var libriAnno = List[Libro]()
    libriAnno = Biblioteca.filter(_.Anno >= 1900)
}

def oldest(Biblioteca: List[Libro]): Libro = {
    Biblioteca.reduceLeft((a, b) => if (a.Anno < b.Anno) a else b)
}

def stampaPerAutore(libriPerAutore:Map[String, List[Libro]]){
    for ((autore, libri) <- libriPerAutore){
        println(s"Autore: $autore")
        for (libro <- libri){
            println(s"${libro.Titolo}")
        }
    }
}
}

<!-- info:start -->
---
    title         : lez01.md
    author        : Kevin
    date          : 12/12/2022
    output        : 
      pdf_document: default
      html_notebook: default
---
<!-- info:end -->

# Scala

<!--toc:start-->
- [Scala](#scala)
  - [Gerarchia dei tipi](#gerarchia-dei-tipi)
    - [Case Class](#case-class)
    - [Options in Scala](#options-in-scala)
    - [Comprehension](#comprehension)
<!--toc:end-->

---

È nato nel 2001, come progetto universitario ma la prima release è del 2003.
Serviva un linguaggio funzionale che sostituisse Java, perché al tempo tanto codice era
scritto in Java ma era diventato stagnante.

> gira sulla **JVM** and **interopera** con le librerie Java

**Scala**ble Language

+ succinto, (a differenza della verbosità di Java), elegante e con una sintassi
  flessibile (50~75% di riduzione di codice)
+ interprete interattivo (**REPL**)
+ supporto per un dominio del linguaggio specifico integrato

> la compilazione di Scala (anche di un semplice HelloWorld) è lento
> È signicativo il tempo necessario per la compilazione

È staticamente tipato!


```scala
  class Upper {
      def upper(strings: String*): Seq[String] = {
          strings.map((s:String)) => s.toUpperCase())
        }
    }

  val up = new Upper
  Console.println(up.upper("A", "First", "Scala", "Program"))
```

Stesso codice, succinto
`object` è un single instance class &rarr; un **Singleton**

```scala
  object Upper {
      def upper(strings: String*) = strings.map(_.toUpperCase())
  }
  println(Upper.upper("A", "First", "Scala", "Program"))
```



Definiamo un **main** per poter compilare il codice precedente!

```scala
  object Upper {
      def main(args: Array[String]) = {
        // la wildcard _ viene utilizzata in due casi che sono indipendenti
        args.map(_.toUpperCase()).foreach(printf("%s ", _))
          println("")
        }
    }
```

Per compilare utilizziamo `scalac <nome_file>` e tutto il codice deve essere in una
classe o in una definizione di un object.

A differenza di Java, la classe stessa è un costruttore
Esempio:

```scala
  class Rational(n: Int, d: Int) extends AnyRef {
    val num = n
    val den = d

    def this(n: Int) = this(n,1)

    // vado a definire un operatore (il this è implicito)
    def + (that: Rational): Rational =
      new Rational(num*that.den + that.num*den, den*that.den)

    def + (i: Int): Rational = new Rational(num+i*den, den)
      override def toString = "" + num + "/" + den
  }
```

In questo caso è possibile chiamare il metodo sul razionale, ma non sull'intero!

Si utilizzerà la **coercion**, definendo i cosidetti **impliciti** che permettono di
promuovere un tipo ad un altro per definire una funzione che stiamo definendo su un
determinato tipo!

## Gerarchia dei tipi

![Type Hiearchy](/Users/Kevin/Documents/@M&K/Personal/Kevin/Informatica/Anno3/Semestre1/Linguaggi di Programmazione/Teoria/Scala/type_hierarcy.png)



> Tutto è un oggetto e ogni operazione è un metodo
> `scala> 1 + 2` &rarr; `1.+(2)`

Per definire una **variabile** come **mutabile** si definisce come `var` e non `val` che
identifica i **valori**

### Case Class

```scala
  abstract class Bool {
    def and(b: => Bool): Bool
    def or(b: => Bool): Bool
  }

  case object True extends Bool {
    def and(b: => Bool) = b
    def or(b: => Bool) = this }

  case object False extends Bool {
    def and(b: => Bool) = this
    def or(b: => Bool) = b
  }

  // Funzione infinita -> ricorsiva
  def bottom: () => Nothing = () => bottom()
```

### Options in Scala

Da un `null` non è possibile sapere che tipo effettivo è l'oggetto!
Per evitare questo, sono state introdotte le **opzioni** (un wrapper) che servono per
gestire il caso di un oggetto


```scala
  // get[variabili di tipo]
  def get[A,B](key: A): Option[B] = {
    if (contains(key)) new Some(getValue(key))
      else None
  }
```


**Metodo**

`val succfun = (x:Int) => x+1` 

**Funzione**

`def succmeth(x: Int) = x+1`

> Il polimorfismo parametrico viene applicato solo ai metodi!

### Comprehension

Sono più forti di quelle di Erlang, ci permettono anche di fare la reduce oltre alla map
e filter.

```scala
  def sum_evens = (L:List[Int]) =>
    {var sum=0; for (X <- L if X%2 == 0) sum += X; sum}
```

Si utilizza lo **Yielding** per ottenere una collection dalla Comprehension

```scala
  val is_prime = (X:Int) => {
    val divisors = (X:Int) =>
      // Questa parte è la Comprehension
      for { Y <- List.range(2,math.sqrt(X).toInt) if (X % Y == 0)} yield Y
    divisors(X).length == 0
  }
```

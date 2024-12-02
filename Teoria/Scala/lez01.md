# Scala


È nato nel 2001, come progetto universitario ma la prima release è del 2003.
Serviva un linguaggio funzionale che sostituisse Java, perché al tempo tanto codice era
scritto in Java ma era diventato stagnante.

> gira sulla **JVM** e **interopera** con le librerie Java

Scala si basa su:

- JVM (Java Virtual Machine), garantendo interoperabilità con le librerie Java.
- Una sintassi concisa ed elegante (riduzione del codice dal 50% al 75%).

Scala è un linguaggio che unisce due paradigmi:
- **Programmazione orientata agli oggetti (OOP)**
- **Programmazione funzionale (FP)**

Scala è tipizzato staticamente e supporta:
- Tipi astratti e path-dependent.
- Classi generiche e metodi polimorfici.
- Inferenza di tipo limitata.

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
### Dettagli:
- `object`: introduce una classe con una singola istanza (singleton).
- `String*`: indica che la funzione può accettare un numero variabile di argomenti.
- `_`: utilizzato come wildcard.

**Compilazione e Esecuzione:**
1. nel terminale scrivere `scala`
2. Salvare il codice in un file `Upper.scala`.
3. Compilare: `scalac Upper.scala`.
4. Eseguire: `scala Upper`.

Per compilare utilizziamo `scalac <nome_file>` e tutto il codice deve essere in una
classe o in una definizione di un object.


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

## Tipi in Scala

Scala ha una gerarchia di tipi con radice in `Any`. Si divide in due sottoinsiemi principali:
- **AnyVal**: tipi di base come `Int`, `Double`, `Boolean`.
- **AnyRef**: tutti i tipi di riferimento, inclusi quelli di Java e Scala.

### Valori speciali:
- `Null`: per i tipi di riferimento (es. `null`).
- `Nothing`: rappresenta un sottoinsieme di tutti i tipi, utile per indicare una lista vuota.

## Gerarchia dei tipi
```
               Any
              /   \
        AnyVal   AnyRef
         /|\       |
        / | \    Object (radice dei tipi di riferimento)
       /  |  \     
    Int  Char Double
     |    |     | 
    Long  Byte Float
     |     |
   Short Boolean
         Unit
          
       Null
          |
      Nothing

```

### Dettagli:
- **Any**: Radice della gerarchia di tutti i tipi in Scala.
Divide i tipi in due categorie principali:
  - **AnyVal**: Contiene i tipi di valore (primitivi).
    - Numerici: Int, Long, Float, Double, Short, Byte, Char.
    - Logici: Boolean.
    - Unit: Equivalente a void in Java, usato per funzioni che non restituiscono un valore.
  - **AnyRef**: Contiene i tipi di riferimento, che includono tutte le classi di Scala e Java.
  Equivalente a Object in Java.
  Contiene tutte le classi definite dall'utente e quelle di Java.
  Da qui derivano:
  Tutte le collezioni Scala (List, Map, Seq, ecc.).
  Tipi personalizzati.

- **Null**: Valore speciale che può essere assegnato ai tipi di riferimento (sottotipo di AnyRef).
Non può essere assegnato ai tipi di valore (AnyVal).

- **Nothing**: Sottotipo di tutti i tipi.
Usato per rappresentare assenza di valore, come nel caso di un'eccezione o di una lista vuota (Nil).


## Paradigma Orientato agli Oggetti

In Scala tutto è un oggetto e ogni operazione è un metodo.

Esempi:
```scala
scala> 1.+(2)
res0: Int = 3

scala> "Ciao".toUpperCase
res1: String = "CIAO"
```

### Variabili:
- **Immutabili**: `val x = 10` (non modificabile).
- **Mutabili**: `var y = 20` (modificabile).

> Tutto è un oggetto e ogni operazione è un metodo
> `scala> 1 + 2` &rarr; `1.+(2)`

## Programmazione Funzionale

Scala permette di trattare le funzioni come valori di prima classe.

Esempio di funzione anonima:
```scala
val addOne = (x: Int) => x + 1
println(addOne(5)) // Output: 6
```

### Polimorfismo nei metodi:
```scala
def identity[T](x: T): T = x
println(identity(42))       // Output: 42
println(identity("Scala"))  // Output: Scala
```

---

## Altre Funzionalità

### Option: gestione sicura dei valori nulli
Esempio:
```scala
val capitals = Map("Italia" -> "Roma", "Francia" -> "Parigi")
println(capitals.get("Italia").getOrElse("Non trovato")) // Output: Roma
```

### Case Class

Le `case class` forniscono un'implementazione automatica di metodi come `toString` e `equals`.

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

```scala
case class Point(x: Int, y: Int)
val p1 = Point(1, 2)
println(p1) // Output: Point(1,2)
```

### Options in Scala

Da un `null` non è possibile sapere che tipo effettivo è l'oggetto!
Per evitare questo, sono state introdotte le **opzioni** (un wrapper) che servono per
gestire il caso di un oggetto

In Scala, al posto di utilizzare `null`, si usa il tipo `Option` per gestire valori opzionali, che può essere:
- **`Some(value)`**: indica che esiste un valore.
- **`None`**: indica l'assenza di un valore.

Evita errori null pointer e rende il codice più leggibile.

```scala
  // get[variabili di tipo]
  def get[A,B](key: A): Option[B] = {
    if (contains(key)) new Some(getValue(key))
      else None
  }
```

## Metodi e Funzioni

**Metodo**

`val succfun = (x:Int) => x+1` 
> Il polimorfismo parametrico viene applicato solo ai metodi!

**Funzione**

`def succmeth(x: Int) = x+1`

sono valori che implementano il metodo `apply`.

### Esempio:
```scala
val add = (x: Int) => x + 1 // Funzione
def addMeth(x: Int): Int = x + 1 // Metodo

println(add(3)) // Output: 4
println(addMeth(3)) // Output: 4
```


## Comprehension

Le comprehension consentono di iterare su una collezione e creare nuovi valori.

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

## Funzioni Notevoli
Scala include funzioni utili come `map`, `reduce`, `exists` e `forall`.

Esempio:
```scala
val nums = List(1, 2, 3, 4)
println(nums.map(_ * 2)) // Output: List(2, 4, 6, 8)
println(nums.reduce(_ + _)) // Output: 10
```

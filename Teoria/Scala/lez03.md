# lez03.md

## Override

In Scala, puoi **sovrascrivere (override)** i membri (`fields` o metodi) definiti nei `trait` o nelle classi base. Ecco alcuni punti fondamentali:

- **Membri astratti**: Un `trait` o una classe può dichiarare membri astratti che devono essere definiti da una classe derivata o da un altro `trait` prima che l'istanza possa essere creata.
- **Parola chiave `override`**: È **obbligatoria** quando sovrascrivi membri concreti, ma **opzionale** per quelli astratti.

### Perché usare `override`?
1. **Evita errori**: Scala segnala errori se il membro da sovrascrivere è scritto male o non esiste.
2. **Protegge dal sovrascrivere accidentalmente**: Non puoi sovrascrivere un membro senza dichiararlo esplicitamente con `override`.

### Accesso al genitore con `super`
Puoi accedere al comportamento originale del membro sovrascritto usando la parola chiave `super`.

### Linearizzazione dei tipi
Scala usa un algoritmo di **linearizzazione** per determinare l'ordine di sovrascrittura quando più `trait` o classi base sono coinvolti.
L'algoritmo funziona così:
1. Inserisce il tipo dell'istanza come primo elemento.
2. Aggiunge i tipi dei `trait` da destra a sinistra (linearizzazione cumulativa).
3. Rimuove i duplicati, mantenendo solo l'ultima occorrenza di ogni tipo.
4. Aggiunge `ScalaObject`, `AnyRef` e `Any`.

```scala
class C1 { def m = List("C1") }
trait T1 extends C1 { override def m = { "T1" :: super.m } }
trait T2 extends C1 { override def m = { "T2" :: super.m } }
trait T3 extends C1 { override def m = { "T3" :: super.m } }
class C2A extends T2 { override def m = { "C2A" :: super.m } }
class C2 extends C2A with T1 with T2 with T3 { override def m = { "C2" :: super.m } }
def linearization(obj: C1, name: String) = {
val lin = obj.m ::: List("ScalaObject", "AnyRef", "Any")
println(name + ": " + lin)
}
```
```cmd
scala> linearization(new C2, "C2 ")
C2 : List(C2, T3, T1, C2A, T2, C1, ScalaObject, AnyRef, Any)
```

Linearization Description
1 C2 add the type of the instance
2 C2, T3, C1 Add the linearization for T3
3 C2, T3, C1, T2, C1 Add the linearization for T2
4 C2, T3, C1, T2, C1, T1, C1 Add the linearization for T1
5 C2, T3, C1, T2, C1, T1, C1, C2A, T2, C1 Add the linearization for C2A
6 C2, T3, T2, T1, C2A, T2, C1 Remove duplicates of C1; all but the last C1
7 C2, T3, T1, C2A, T2, C1 Remove duplicates of T2; all but the last T2
8 C2, T3, T1, C2A, T2, C1, ... Finish!


## Overridando i campi nelle classi

Puoi anche sovrascrivere campi (variabili o valori). Tuttavia, se sovrascrivi un `val`, devi specificare `override`.

```scala
  trait T1 { val name = "T1" }
  class Base
  class ClassWithT1 extends Base with T1 { override val name = "ClassWithT1" }

  val c = new ClassWithT1()
  println(c.name)

  class ClassExtendsT1 extends T1 { override val name = "ClassExtendsT1" }

  val c2 = new ClassExtendsT1()
  println(c2.name)
```
```cmd
[DING!]cazzola@surtur:~/lp/scala>scala val-override.scala
ClassWithT1
ClassExtendsT1
```

```scala
class C1 { val name = "C1"; var count = 0 }
class ClassWithC1 extends C1 { override val name = "ClassWithC1"; count = 1 }
val c = new ClassWithC1()
println(c.name, c.count)
```
```cmd
[15:10]cazzola@surtur:~/lp/scala>scala val-override-inclass.scala
(ClassWithC1,1)
```
```scala
import java.io._
abstract class BulkReader {
type In
val source: In
def read: String
}
class StringBulkReader(val source: String) extends BulkReader {
type In = String
def read = source
}
class FileBulkReader(val source: File) extends BulkReader {
type In = File
def read = {
val in = new BufferedInputStream(new FileInputStream(source))
val numBytes = in.available()
val bytes = new Array[Byte](numBytes)
in.read(bytes, 0, numBytes)
new String(bytes)
}
}
println( new StringBulkReader("Hello Scala!").read )
println( new FileBulkReader(new File("BulkReader.scala")).read )
```
```cmd
[15:34]cazzola@surtur:~/lp/scala>scala BulkReader.scala
Hello Scala!
import java.io._
...
```

## Oggetti associati
Quando una **classe** e un **oggetto** hanno lo stesso nome e si trovano nello stesso package, sono considerati **associati**. 

- Il nome della classe è memorizzato nel "namespace dei tipi".
- Il nome dell'oggetto è memorizzato nel "namespace dei termini".
- Non c'è conflitto tra i due.

### Metodo `apply`

Se un'istanza è seguita da parentesi (anche vuote), il compilatore chiama il metodo `apply` della classe o dell'oggetto.

Questo vale sia per un oggetto o un'istanza di una classe che definisce apply.

 ```scala
 type Pair[+A, +B] = Tuple2[A, B]
object Pair {
def apply[A, B](x: A, y: B) = Tuple2(x, y)
def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
}
```
questo permette di creare Pair
 ```scala
 val p = Pair(1, "one")
  ```

### metodo `unapply`
Utilizzato per estrarre le parti costituenti di un istanza, è molto usato nei pattern matching

 ```scala
object Twice { def unapply(z: Int): Option[Int] = if (z%2 == 0) Some(z/2) else None }
object TwiceTest extends Application {
val x = 42; x match { case Twice(n) => Console.println(n) }
}
 ```
  ```cmd
scala> TwiceTest
21
res8: TwiceTest.type = TwiceTest$@3b2601c
 ```

### Apply & UnapplySeq for collections
Possono essere utilizzati per costruire una collezione da una lista di argomenti variabili o per estrarre i primi elementi da una collezione.

```scala
object L2 {
def unapplySeq(s: String) : Option[List[String]] = Some(s.split(",").toList)
def apply(stuff: String*) = stuff.mkString(",")
}
```
```cmd
scala> val x2 = L2("4", "5", "6")
x2: String = 4,5,6
scala> val L2(d,e,f) = x2
d: String = 4
e: String = 5
f: String = 6
```

## Classi Case

sono classi che esportano i parametri del loro costruttore che fornisce un meccanismo di decomposizione ricorsivo attraverso il pattern matching.

```scala
 abstract class Term
case class Var(name: String) extends Term
case class Fun(arg: String, body: Term) extends Term
case class App(f: Term, v: Term) extends Term
  ```
Le **case class** hanno caratteristiche speciali:
1. I parametri del costruttore sono trattati come **val** pubblici.
2. Generano automaticamente i metodi `equals`, `hashCode` e `toString`.
3. Forniscono il metodo `copy` per creare copie modificate.

```scala
object TermTest extends Application {
def printTerm(term: Term) {
term match {
case Var(n) => print(n)
case Fun(x, b) => print("ń" + x + "."); printTerm(b)
case App(f, v) => Console.print("("); printTerm(f)
print(" "); printTerm(v); print(")")
}
}
def isIdentityFun(term: Term): Boolean = term match {
case Fun(x, Var(y)) if x == y => true
case _ => false
}
val id = Fun("x", Var("x"))
val t = Fun("x", Fun("y", App(Var("x"), Var("y"))))
printTerm(t); println; println(isIdentityFun(t))
printTerm(id); println; println(isIdentityFun(id))
}
```
```cmd
[15:16]cazzola@surtur:~/lp/scala>scala TermTest.scala
ńx.ńy.(x y)
false
ńx.x
true
```
trait monoide[A] {
  def add(a: A, b: A): A
  val identity: A


  def isAssociative(a: A, b: A, c: A) =
    add(a, add(b, c)) == add(add(a, b), c)
  

  def hasIdentity(x: A) =
    add(x, identity) == x && add(identity, x) == x
  
}

trait gruppo[A] extends monoide[A]{
  def inverse(a: A): A

  def hasInverse(x: A): Boolean = {
    add(x, inverse(x)) == identity
  }
  
  def isCommutative(a: A, b: A): Boolean ={
    add(a, b) == add(b, a)
  }
}

trait ring[A] extends gruppo[A]{
    def mul(a: A, b: A): A    
    def mulIdentity: A
    def isDistributive (a:A, b: A, c: A): Boolean ={
        mul(a, mul(b, c)) == mul(mul(a, b), mul(a, c)) && mul(mul(a, b), c) == mul(mul(a, c), mul(b, c))
    }

    def isMulAssociative(a: A, b: A, c: A): Boolean ={
        mul(a, mul(b, c)) == mul(mul(a, b), c)
    }

    def isMulIdentity(x: A): Boolean ={
        mul(x, mulIdentity) == x && mul(mulIdentity, x) == x
    }
}
object Main extends App {
    val t = true
    val f = false
    val a = 2
    val b = 3
    val c = 4

    object BoolMonoide extends monoide[Boolean] {
        def add(t: Boolean, f: Boolean): Boolean = t || f
        val identity: Boolean = false
    }

    object IntMonoide extends monoide[Int] {
        def add(a: Int, b: Int): Int = a + b
        val identity: Int = 0
    }


    object BoolGruppo extends gruppo[Boolean] {
        def add(t: Boolean, f: Boolean): Boolean = t || f
        val identity: Boolean = false
        def inverse(a: Boolean): Boolean = !a
    }

    object IntGruppo extends gruppo[Int] {
        def add(a: Int, b: Int): Int = a + b
        val identity: Int = 0
        def inverse(a: Int): Int = -a
    }

    object IntRing extends ring[Int] {
        def add(a: Int, b: Int): Int = a + b
        val identity: Int = 0
        def inverse(a: Int): Int = -a
        def mul(a: Int, b: Int): Int = a * b
        val mulIdentity: Int = 1
    }
    println("MONIDE")
    println(s"BoolAdd: ${BoolMonoide.add(t, f)}")
    println(s"IntAdd: ${IntMonoide.add(a, b)}")

    println("\nGRUPPO")
    println(s"BoolAdd: ${BoolGruppo.add(t, f)}")
    println(s"BoolInverse: ${BoolGruppo.inverse(t)}")
    println(s"IntAdd: ${IntGruppo.add(a, b)}")
    println(s"IntInverse: ${IntGruppo.inverse(a)}")
    println(s"Is Associative: ${IntGruppo.isAssociative(a, b, c)}")
    println(s"Has Identity: ${IntGruppo.hasIdentity(a)}")
    println(s"Has Inverse: ${IntGruppo.hasInverse(a)}")
    println(s"Is Commutative: ${IntGruppo.isCommutative(a, b)}")


    println("\nRING")
    println(s"Add: ${IntRing.add(a, b)}")
    println(s"Inverse: ${IntRing.inverse(a)}")
    println(s"Mul: ${IntRing.mul(a, b)}")
    println(s"Is Associative: ${IntRing.isAssociative(a, b, c)}")
    println(s"Has Identity: ${IntRing.hasIdentity(a)}")
    println(s"Has Inverse: ${IntRing.hasInverse(a)}")
    println(s"Is Commutative: ${IntRing.isCommutative(a, b)}")
    println(s"Is Distributive: ${IntRing.isDistributive(a, b, c)}")
    println(s"Is Mul Associative: ${IntRing.isMulAssociative(a, b, c)}")
    println(s"Is Mul Identity: ${IntRing.isMulIdentity(a)}")
}
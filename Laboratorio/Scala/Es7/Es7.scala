import scala.util.Random

object matrix{
    def build (x: Int, y: Int): Array[Array[Int]] = {
        Array.fill(x, y)(Random.nextInt(10))
    }

    def equivalence (a: Array[Array[Int]], b: Array[Array[Int]]): Boolean ={
        if (a.length != b.length || a(0).length != b(0).length) {
            false
        } else {
            val diversi = for {
                i <- 0 until a.length
                j <- 0 until b(0).length
                if a(i)(j) != b(i)(j)
            } yield false
        
            diversi.isEmpty
        }
    }

    def copy(a: Array[Array[Int]]): Array[Array[Int]] ={
        val b: Array[Array[Int]] = Array.ofDim[Int](a.length, a(0).length)
        for (i <- a.length -1 to 0 by -1){
            for (j <- a(i).length -1 to 0 by -1){
                b(i)(j) = a(i)(j)
            }
        }
        b
    }

    def addition (a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] ={
        if (a.length != b.length || a(0).length != b(0).length) return null
        val res: Array[Array[Int]] = Array.ofDim[Int](a.length, b(0).length)
        for (i <- a.length -1 to 0 by -1){
            for (j <- a(i).length -1 to 0 by -1){
                res(i)(j) = a(i)(j) + b(i)(j)
            }
        }
        res
    }

    def scalarMul (a: Array[Array[Int]], b: Int): Array[Array[Int]]={
        val c: Array[Array[Int]] = Array.ofDim[Int](a.length, a(0).length)
        for (i <- a.length -1 to 0 by -1){
            for (j <- a(i).length -1 to 0 by -1){
                c(i)(j) = a(i)(j) * b
            }
        }
        c
    }

    def matrixMul (a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] ={
        if (a(0).length != b.length) {
        throw new IllegalArgumentException("Le matrici non sono moltiplicabili: il numero di colonne della prima matrice deve essere uguale al numero di righe della seconda")
        }
    
        val res: Array[Array[Int]] = Array.ofDim[Int](a.length, b(0).length)
        for (i <- 0 until a.length) {
            for (j <- 0 until b(0).length) {
                res(i)(j) = 0
                for (k <- 0 until b.length) {
                    res(i)(j) += a(i)(k) * b(k)(j)
                }
            }
        }
        res
    }

    def transposition (a: Array[Array[Int]]): Array[Array[Int]]={
        val b: Array[Array[Int]] = Array.ofDim[Int](a(0).length, a.length)
        for (i <- a.length -1 to 0 by -1){
            for (j <- a(i).length -1 to 0 by -1){
                b(j)(i) = a(i)(j)
            }
        }
        b
    }

    def norm1(a: Array[Array[Int]]): Int = {
        //fatto da copilot
        val cols = a(0).length
        val rows = a.length
        var maxSum = 0
        for (j <- 0 until cols) {
            var colSum = 0
            for (i <- 0 until rows) {
                colSum += math.abs(a(i)(j))
            }
            if (colSum > maxSum) {
                maxSum = colSum
            }
        }
        maxSum
    }

    def printMat (a: Array[Array[Int]]): Unit = {
        for (i <- 0 until a(0).length) {
            for (j <- 0 until a.length) {
                print(a(j)(i) + " ")
            }
            println()
        }
    }

    def main (args: Array[String]): Unit ={
        val a : Array[Array[Int]] = build(3, 4)
        val b : Array[Array[Int]] = build(4, 3)
        val d : Array[Array[Int]] = build(3, 4)

        val c = copy(a)

        println("a:\n")
        printMat(a)

        println("\nb:\n")
        printMat(b)

        println("\nc:\n")
        printMat(c)

        println("\nd:\n")
        printMat(d)

        println("\na uguale a b:\n")
        println(equivalence(a, b))

        println("\na uguale a c:\n")
        println(equivalence(a, c))

        println("\na + d:\n")
        printMat(addition(a, d))

        println("\na * 3:\n")
        printMat(scalarMul(a, 3))

        println("\na * b:\n")
        printMat(matrixMul(a, b))

        println("\ntransposizione di a:\n")
        printMat(transposition(a))

        println("\nnorm1 di a")
        println(norm1(a))
    }
}
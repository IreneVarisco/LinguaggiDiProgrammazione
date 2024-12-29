object es6 {
    def goldbach(n: Int): String = {
        if (n < 4 || n % 2 != 0) {
            return "risultato non disponibile"
        }else {
            val result = for {
                i <- 2 to n/2
                j = n - i
                if isPrimo(i) && isPrimo(j)
            } yield s"$i + $j"
            
            result.headOption.getOrElse("risultato non disponibile")
        }
        /*for (i <- 2 to n / 2) {
            val j = n - i
            if (isPrimo(i) && isPrimo(j)) {
                return i + " + " + j
            }
        }
        "risultato non disponibile"*/
    }

    def isPrimo(n: Int): Boolean = {
        if (n <= 1) false
        else if (n == 2) true
        else if (n % 2 == 0) false

        else{
            val radice = math.sqrt(n).toInt
            !(3 to radice by 2).exists(i => n % i == 0)
        }
    }
    
    def goldbach_list(n: Int, m: Int): List[String] = {
        val res = for (i <- n to m by +1 if i % 2 == 0) yield goldbach(i)
        res.toList
    }

    def main(args: Array[String]): Unit = {
        println(goldbach(18))
        println()
        for(j<-goldbach_list(7,13)) println(j)
    }
}
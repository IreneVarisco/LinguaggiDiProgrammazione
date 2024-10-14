object ParseExpr extends Arith {
	def main(args: Array[String]) = {
		println("input : "+ args(0))
		println(parseAll(expr, args(0)))
	}
}


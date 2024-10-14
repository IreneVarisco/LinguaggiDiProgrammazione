class Primes {
  lazy val prime: LazyList[Int] = 2 #:: LazyList.from(3).filter {
  x => !prime.takeWhile(y * y <= x).exists(i => x % i == 0)}
}

object Euler7 extends App {

  def euler7(n: Int){}

  def isPrime(num: Int) = {
    //2 to math.sqrt(num).toInt
    !(2 to num-1).exists(num % _ == 0)
  }

  def nextPrimeFrom(num: Int) = {
    //Iterators do not store all the numbers before the searched one
    Iterator.from(num + 1).find(isPrime).get
  }

  //Infinite series of prime numbers starting from 2
  val primes = Iterator.iterate(2)(nextPrimeFrom) //infinite series
  //drop the first 10000 values and take the next
  println(primes.drop(10000).next)
  //println(primes.drop(5000).next)
}

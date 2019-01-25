object BigEuler extends  App {

  /**
    * Problem 1
    *
    * Add all the natural numbers below
    * one thousand that are multiples of 3 or 5.*
    *
    * @param n
    * @return
    */
  def sumUpToDivisorOf3And5(): Int = {
    (0 to 1000).filter((a) => (a % 3 == 0 || a % 5 == 0)).sum
  }

  /**
    * Problem 2
    *
    * Find the sum of all the even-valued terms
    * in the Fibonacci sequence which do not exceed
    * four million.
    */

  def fib (a :Int, b :Int) :Int =
    (if (b % 2 == 0) b else 0) + (if (a+b > 1000000) 0 else fib(b, a+b))
  def answer = fib(1, 2)


  /**
    * Problem 3
    * Find the largest prime factor of a composite number
    */
  def lpf (divis :Long, divid :Long) :Long = {
    if (divid % divis == 0) lpf(2, divid/divis)
    else if (divis > math.sqrt(divid)) divid
    else lpf(divis+1, divid)
  }

  /**
    * Problem 4
    *
    * Find the largest
    * palindrome made from the product of two 3-digit numbers
    */
  def palindrome (value :String) =
    value.substring(0, value.length/2) == value.substring(value.length/2).reverse

  def findLargestPalindrome(): Unit ={
    (for (a <- 100 until 999 ; b <- 100 until 999
          if palindrome(a * b toString))
      yield a*b).max
  }

  /**
    * Problem 5
    *
    * What is the smallest
    * number divisible by each of the numbers 1 to 20?
    */

  val r = Range(20, Int.MaxValue)
    .find(n => Range(2, 21).forall(n % _ == 0)).get
  assert(r == 232792560) // 23 s

  /*
  Problem 6
What is the difference between the
sum of the squares and the square of the sums?*
   */
  val numbers = 1 to 100
  def square(n: Int) = n * n
  square(numbers.sum) - numbers.map(square).sum
  //assert(r == 25164150) // 1 ms

  /**
    * Problem 7
    * Find the 10001st prime
    */
  def prime1001 = {
    var numbers = List.range(2,110000);
    var primes = List(2);
    while (primes.length < 10001) {
      val prime = primes.head;
      numbers = numbers.filter((b) => (b%prime != 0));
      primes = numbers.head :: primes;
    }
    primes.head
  }

  /*
  Problem 14
  Find the longest sequence using a starting number under
  1 million
   */

  def from(n: Long, c: Int = 0): Int = if (n == 1) c + 1 else
    from(if (n % 2 == 0) n / 2 else 3 * n + 1, c + 1)
  val res = (1 until 1000000).view.map(n => (n, from(n)))
    .reduceLeft((a, b) => if (a._2 > b._2) a else b)._1
  assert(res == 837799)

  /**
    * Problem 16
    * Sum of the digit of a number with pow (2 hoch 1000)
    */
  val res1 = BigInt(2).pow(1000).toString.view.map(_.asDigit).sum


  /**
    * Problem 20
    * Find the sum of the digits in 100!
    */

  def f(n: BigInt): BigInt = if (n < 2) 1 else n * f(n - 1)
  val res2 = f(100).toString.view.map(_.asDigit).sum
  assert(res2 == 648) // 1 ms

  /**
    * Problem 30
    * Find the sum of all the numbers that can be written as
    * the sum of fifth powers of their digits
    */
  def max(d: Int) = math.pow(10, d).toInt - 1
  def sum(n: Int) = n.toString.map(_.asDigit)
    .map(math.pow(_, 5).toInt).sum
  val limit = Stream.from(1).find(d => max(d) > sum(max(d))).get
  val res4 = (2 to max(limit)).view.filter(n => n == sum(n)).sum
  assert(res4 == 443839) // 2 s


}

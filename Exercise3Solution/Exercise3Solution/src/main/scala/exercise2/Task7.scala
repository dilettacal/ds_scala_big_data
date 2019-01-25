package exercise2

import scala.annotation.tailrec

object Task7 extends App {

  def primSum(max: Int): Int = {
    def isPrime(x: Int): Boolean = {
      @tailrec
      def isPrime(x: Int, i: Int, max: Int): Boolean = {
        if (i >= max) true
        else if (x % i == 0)
          false
        else
          isPrime(x, i + 1, max)
      }

      isPrime(x, 2, Math.sqrt(x).toInt + 1)
    }

    @tailrec
    def primSum(current: Int, max: Int, sum: Int): Int = {
      if (current > max)
        sum
      else if (isPrime(current)) {
        primSum(current + 1, max, sum + current)
      } else
        primSum(current + 1, max, sum)
    }

    primSum(1, max, 0)
  }

  println(primSum(2000000))
}

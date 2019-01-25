package exercise2

import utils.TimeUtils

/**
  * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  *
  * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  *
  * https://projecteuler.net/problem=5
  */
//Note, the solutions here are far from optimal
object Problem5 {

  //returns true if the given number is evenly divisible by all numbers up to n
  private def isDivisible(number: Int, n: Int): Boolean = {
    for (i <- 2 to n) {
      if (number % i != 0)
        return false
    }
    true
  }

  //start at 1 and incrementally find the first number divisible by all using a while loop
  def bruteForceImperativ(n: Int): Int = {
    var current = 1
    while (!isDivisible(current, n)) {
      current += 1
    }
    current
  }

  //start at n and incrementally find the first number
  // divisible by all using recursion and steps of 20 instead of 1
  def bruteForceRecursion(n: Int): Int = {
    def bruteForceRec(current: Int, n: Int): Int = {
      if (isDivisible(current, n))
        current
      else
        bruteForceRec(current + n, n)
    }

    bruteForceRec(n, n)
  }

  //Same as the non-recursive brute force solution,
  //but using elements of functional programming instead of a while loop
  def bruteForceFunctional(n: Int): Int = {
    val range = (2 to n).toArray
    (1 to Int.MaxValue).find(x => range.forall(i => x % i == 0)).get
  }

  def main(args: Array[String]): Unit = {
    val n = 20
    TimeUtils.time {
      println(bruteForceImperativ(n))
    }

    TimeUtils.time {
      println(bruteForceFunctional(n))
    }

    TimeUtils.time {
      println(bruteForceRecursion(n))
    }
  }
}
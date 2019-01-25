package exercise2

import utils.TimeUtils

import scala.collection.mutable.ListBuffer

/**
  * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
  *
  * What is the 10001st prime number?
  *
  * https://projecteuler.net/problem=7
  */
//Note, the solutions here are far from optimal
object Problem7 {

  //Start at 2 (the first prime number) and execute a prime test n times to find the nth prime
  def bruteForceImperativ(n: Int): Int = {

    //Check if the given number is prime
    // by checking if it is divisible by no smaller numbers up to its square root
    def isPrime(number: Int): Boolean = {
      for (i <- 2 to Math.sqrt(number).toInt) {
        if (number % i == 0)
          return false
      }
      true
    }

    var numPrimesFound = 0
    var current = 1
    while (numPrimesFound != n) {
      current += 1
      if (isPrime(current))
        numPrimesFound += 1
    }
    current
  }

  //Start at 2 (the first prime number) and execute a prime test n times to find the nth prime
  def divisionWithPrimesImperativ(n: Int): Int = {
    var foundPrimes = ListBuffer.empty[Int]

    //Check if the given number is prime
    // by checking if it is divisible by all smaller prime numbers up to its square root
    def isPrime(num: Int, primes: Seq[Int]): Boolean = {
      for (prime <- primes) {
        if (prime > Math.sqrt(num))
          return true
        if (num % prime == 0)
          return false
      }
      true
    }

    var current = 1
    while (foundPrimes.size != n) {
      current += 1
      if (isPrime(current, foundPrimes))
        foundPrimes += current
    }
    current
  }

  def main(args: Array[String]): Unit = {
    val n = 10001
    TimeUtils.time {
      println(bruteForceImperativ(n))
    }
    TimeUtils.time {
      println(divisionWithPrimesImperativ(n))
    }
  }
}

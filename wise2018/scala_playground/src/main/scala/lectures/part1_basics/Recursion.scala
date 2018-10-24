package lectures.part1_basics

import com.sun.corba.se.spi.monitoring.StatisticsAccumulator

import scala.annotation.tailrec

object Recursion extends App {

  def factorial(n: Int): Int =  {
    if (n <= 0) 1 else{
      println("Factorial of " + n + " - First factorial of " + (n-1) + " is needed")
      val result = n*factorial(n-1)
      println("Computed factorial of " + n)
      result
    }
  }

  println("Call stack of a recursive function")
  println(factorial(10))

  def anotherFactorial(n: Int): BigInt = {
    @tailrec
    def factHelper(x: Int, accumulator: BigInt): BigInt = {
      if (x <= 1) accumulator
        //no additional stack frame is required
        //this is the last call --> computes only intermediate results
      else factHelper(x -1, x*accumulator) //TAIL Recursion = use recursive call as the last expression
    }
    factHelper(n, 1)
  }
  /*
    anotherFactorial(10) = factHelper(10,1)
    factHelper(9, 10*1)
    factHelper(8, 9*10*1)
    factHelper(7, 8*9*10*1)
      ...
   */
  println(anotherFactorial(10))
  //println(anotherFactorial(5000)) //0 but not StackOverflowError --> bigint: computation works!

  /*
    1. String concatenation function with tail recursion
    2. IsPrime function tail recursive
    3. Fibonacci tail recursive
   */

  @tailrec
  def concatenateTailrec(aString: String, n: Int, accumulator: String): String = {
    if(n <= 0) accumulator
    else concatenateTailrec(aString, n-1, aString+accumulator)
  }

  println(concatenateTailrec("Hallo", 3, " "))

  def isPrime(n: Int): Boolean = {
    def isPrimeTailrec(t: Int, isStillPrime: Boolean): Boolean ={
      if(!isStillPrime) false
      else if (t <= 1) true
      else isPrimeTailrec(t-1, n%t != 0 && isStillPrime)
    }
    isPrimeTailrec(n/2, true)
  }

  println(isPrime(2003))

  def fibonacci(n: Int): Int = {
    //n-1, n-2
    def fiboTailRec(i: Int, last: Int, nextToLast: Int): Int ={
      if(i >= n) last
      else fiboTailRec(i+1,last + nextToLast, last)
    }
    if (n<= 2) 1
    else fiboTailRec(2,1,1)
  }
  println(fibonacci(8))
}

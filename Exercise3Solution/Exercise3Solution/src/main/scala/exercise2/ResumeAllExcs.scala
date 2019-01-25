package exercise2

import utils.TimeUtils

import scala.annotation.tailrec

object ResumeAllExcs extends App {

  //DIGIT SUM
  def digitSum(x: Int): Int = x match {
    case 0 => 0
    case _ => x % 10 + digitSum(x / 10)
  }

  //DIGIT SUM TAILREC
  def digitSumTailRec(x: Int): Int = {
    @tailrec
    def digitSumTailRec(x: Int, sum: Int): Int = x match {
      case 0 => sum
      case _ => digitSumTailRec(x / 10, sum + x % 10)
    }

    digitSumTailRec(x, 0)
  }

  println(digitSum(5320))

  println(digitSumTailRec(5320))

  //FINDMIN TAIL REC
  def findMin(xs: Array[Int]): Int = {

    def findMin(x: Int, xs: Array[Int]): Int = xs match {
      //alternative ways of checking, if the array is empty, would be
      //case y if y.IsEmpty => x
      //case y if y.length == 0 => x
      case Array() => x
      case _ => findMin(Math.min(x, xs.head), xs.tail)
    }

    if (xs.isEmpty)
      -1
    else
      findMin(Int.MaxValue, xs)
  }

  println(findMin(Array(5, 2, 3, 1, 4)))

  println(findMin(Array()))


  //FIBONACCI
  def fibo(n: Long): Long = n match {
    case 1 => 0
    case 2 => 1
    case _ => fibo(n - 1) + fibo(n - 2)
  }

  println(TimeUtils.time(fibo(5)))
  println(TimeUtils.time(fibo(30)))

  def fibo2(n: Int): Int = {
    def fibo2(n: Int, prev: Int, prev2: Int): Int = {
      n match {
        case 1 => prev
        case _ => fibo2(n - 1, prev2, prev + prev2)
      }
    }

    fibo2(n, 0, 1)
  }

  println(TimeUtils.time(fibo2(30)))

  //PALINDROME

  def isPalindrome(s: String): Boolean = s match {
    case _ if s.isEmpty => true
    case _ if s.length == 1 => true
    case _ if s.head != s.last => false
    case _ => isPalindrome(s.substring(1, s.length - 1))
  }

  def nMissingParenthesis(s: String): Int = {
    def inner(n: Int, s: String): Int = s match {
      case _ if s.isEmpty => n
      case _ if s.head == '{' => inner(n + 1, s.tail)
      case _ if s.last == '}' => inner(n - 1, s.tail)
    }

    inner(0, s)
  }

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

}

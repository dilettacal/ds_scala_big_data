package functional_programming.starter

import scala.annotation.tailrec

object Arrays extends App {

  val x = Array(1,2,3,4,5,6)

  //imperative
  println("Imperative way: ")
  for(a <- x) print(a + " ")

  println()
  println("Better way")
  x.foreach(a => print(a + " "))

  println("Counting elements")
  println(x.count(_ % 2 == 0))

  println("Recursion for immutability")

  def factorial(k: Int):Int = {
    @tailrec
    def fact(n:Int, acc:Int): Int = n match {
      case 1 => acc
      case _ => fact(n-1, n*acc)
    }
    fact(k,1)
  }

  println(factorial(5))

  val y = 1 to 10 //Range

  println(y.map(_/2).filter(_ > 0).partition(_ < 2))

  println("Lists: ")
  val list = (1 to 100).toList.view //SeqView, lazy collection

  def isDivisibleBy(d: Int)(n:Int) = {
    println(s"Checking ${n} by ${d}")
    n%d == 0
  }

  //Create functions
  val by2 = isDivisibleBy(2)(_)
  val by3 = isDivisibleBy(3)(_)
  val by5 = isDivisibleBy(5)(_)

  val result = list.filter(n => by2(n)).filter(n => by3(n)).filter(n => by5(n)).toList //all numbers divisible by 2,3,5
  println(result)


}

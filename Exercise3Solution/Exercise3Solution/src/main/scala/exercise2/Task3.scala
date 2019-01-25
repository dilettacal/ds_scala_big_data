package exercise2

import utils.TimeUtils

object Task3 extends App {

  def fibo(n: Long): Long = n match {
    case 1 => 0
    case 2 => 1
    case _ => fibo(n - 1) + fibo(n - 2)
  }

  println(TimeUtils.time(fibo(5)))
  println(TimeUtils.time(fibo(30)))
}

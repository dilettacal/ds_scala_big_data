package exercise2

import utils.TimeUtils

object Task4 extends App {
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
}

package exercise2

import scala.annotation.tailrec

object Task1 extends App {

  def digitSum(x: Int): Int = x match {
    case 0 => 0
    case _ => x % 10 + digitSum(x / 10)
  }

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
}

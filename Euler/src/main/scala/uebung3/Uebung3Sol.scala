package uebung3

object Uebung3Sol extends App {

  def digitSum(x: Int): Int = x match {
      case 0 => 0
      case _ => x % 10 + digitSum(x / 10)
    }


  def findMin(ints: Array[Int]): Int = {
    def findMin(x: Int, xs: Array[Int]): Int =
      xs match {
      case Array() => x
      case _ => findMin(Math.min(x, xs.head), xs.tail)
    }
    findMin(Int.MaxValue, ints) match {
      case Int.MaxValue => -1
      case x => x
    }
  }

  def fiboRec(n: Int): Int = -1

  def fiboTailRec(n: Int): Int = -1

  def isPalyndrom(str: String): Boolean = true


}

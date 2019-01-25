package exercise2

object Task2 extends App {

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
}
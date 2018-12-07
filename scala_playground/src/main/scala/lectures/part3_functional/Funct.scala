package lectures.part3_functional

object Funct {

  //use functions as first class elements
  //problem: oop

  def main(args: Array[String]): Unit ={
    val doubler = new MyFunction[Int, Int] {
      override def apply(element: Int): Int = element*2
    }

    println(doubler(4))
  }


}

trait MyFunction[A,B] {
  def apply(element: A): B = ""
}

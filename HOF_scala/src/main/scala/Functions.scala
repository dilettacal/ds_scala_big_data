object Functions extends  App {

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int =
      element * 2
  }

  println(doubler(3))

  //function types: Function - supports up to 22 params
  val stringToIntConverter = new Function1[String, Int] {
    override def apply(v1: String): Int =
      v1.toInt
  }
  println(stringToIntConverter("3") + 4)

  val adder = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int =
      v1+v2
  }
  /**
    * 1. A function which takes 2 strings and concatenate them
    */
  val stringConcatenator = new Function2[String, String, String]{
    override def apply(v1: String, v2: String): String = v1+v2
  }

  println(stringConcatenator("Hallo", "World"))

  /**
    * 2. T
    */
}
trait MyFunction[A,B]{
  def apply(element:A ): B
}
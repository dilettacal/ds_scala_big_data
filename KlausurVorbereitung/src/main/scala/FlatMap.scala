object FlatMap extends App {

  val fruits = Seq("apple", "banana", "orange")
  println(fruits.map(_.toUpperCase)) // List(APPLE, BANANA, ORANGE)
  //FlatMap flattens the resulting list into a sequence of Char
  println(fruits.flatMap(_.toUpperCase)) //
  // List(A, P, P, L, E, B, A, N, A, N, A, O, R, A, N, G, E)
  println(fruits.map(_.toUpperCase).flatten) //same

  def toInt(s: String): Option[Int] = {
    try {
      Some(Integer.parseInt(s.trim))
    } catch {
      case e: Exception => None
    }
  }

  val strings = Seq("1", "2", "foo", "3", "bar")
  println(strings.map(toInt))//List(Some(1), Some(2), None, Some(3), None)
  println(strings.flatMap(toInt)) //List(1, 2, 3) - None are removed!
  println(strings.flatMap(toInt).sum) //6
  println(strings.map(toInt).flatten.sum) //6 the same with flatmap


  //Conversion Map to FlatMap
  val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
  println(1 to map.size flatMap(map.get))//Vector(one, two, three)
}

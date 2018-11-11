package list.app

import assignment1.Problems
import list.implementation.{Cons, Empty, SinglyLinkedIntList}

object App extends App {
  val x = 5 + 4 +12
  println(x.toFloat)
  println(math.ceil(x.toFloat/10))

  val c = Problems.countChange(10, SinglyLinkedIntList(1,2))
  println(c)

}

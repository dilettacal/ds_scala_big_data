package list.app

import list.implementation.{Cons, Empty}

object App extends App {
  val x = Cons(1,Cons(2,Empty))
  println(x)

}

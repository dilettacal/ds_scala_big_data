package app

import assignment1.Problems
import list.implementation.{Cons, Empty, SinglyLinkedIntList}
import list.traits.IntList

object App extends App {
  val list1 = List(1,2,3,5,7,10,13)
  val lits2 = List("A", "B", "C")

  //apply a binary operator at each element
  //Result of a step is passed to the next

  println(lits2.reduceLeft(_ + _))

  println(list1.reduceLeft(_ + _))

  println(list1.reduceLeft((x,y) => {println(x + ", " + y); x+y}))
  println()
  println(list1.reduceRight((x,y) => {println(x + ", " + y); x+y}))
  println()

  println(list1.reduceLeft(_ - _))
  println(list1.reduceRight(_ - _))

  println()
  println(list1.foldLeft(1)(_ + _))
  println(lits2.foldLeft("z")(_ + _))
  println(lits2.foldRight("z")(_ + _))

  val testList = Cons(3,Cons(4,Cons(10, Cons(6, Empty))))
  println(testList.delete(10))




}

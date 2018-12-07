import java.util.NoSuchElementException

abstract class MyList {

  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(element: Int): MyList
  def printElements: String

  override def toString: String = "["  +printElements + "]"
}

object Empty extends MyList{
  override def head: Int = throw new NoSuchElementException("No such Element!")
  override def tail: MyList = throw new NoSuchElementException("No such Element!")
  override def isEmpty: Boolean = true
  override def add(element: Int): MyList = new Cons(element, Empty)

  override def printElements: String = ""
}

class Cons(h:Int, t:MyList) extends MyList{
  override def head: Int = h
  override def tail: MyList = t
  override def isEmpty: Boolean = false
  override def add(element: Int): MyList = new Cons(element, this)

  override def printElements: String =
    if(tail.isEmpty) "" + h
    else h +  " " + t.printElements
}


object ListTest extends App{
  val list = new Cons(1, new Cons(2,new Cons(4, Empty)))
  println(list.tail)
  println(list.add(5))
}
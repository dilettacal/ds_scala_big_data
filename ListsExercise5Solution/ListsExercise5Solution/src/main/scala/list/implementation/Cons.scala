package list.implementation

import list.traits.IntList

case class Cons(head: Int, tail: IntList) extends SinglyLinkedIntList {
  override def isEmpty = false

  override def get(index: Int): Int = index match {
    case 0 => head
    case i => tail.get(i - 1)
  }

  override def append(elem: Int): IntList = Cons(head, tail.append(elem))

  override def contains(elem: Int): Boolean = elem match {
    case _ if elem == head => true
    case _ => tail.contains(elem)
  }

  override def delete(elem: Int): IntList = elem match{
    case _ if elem == head => tail
    case _ => Cons(head,tail.delete(elem))
  }

  override def deleteAll(elem: Int): IntList = elem match{
    case x if x == head => tail.deleteAll(elem)
    case _ => Cons(head,tail.deleteAll(elem))
  }

  override def prefix(other: IntList): IntList = other match{
    case Empty => this
    case _ => Cons(other.head,prefix(other.tail))
  }
}
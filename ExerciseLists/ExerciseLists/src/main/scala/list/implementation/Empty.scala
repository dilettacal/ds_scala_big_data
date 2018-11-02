package list.implementation

import list.traits.IntList

case object Empty extends SinglyLinkedIntList {

  override def head: Int = throw new IllegalArgumentException("head.nil")

  override def tail: IntList = throw new IllegalArgumentException("tail.nil")

  override def isEmpty = true

  override def get(index: Int) = throw new IndexOutOfBoundsException()

  override def append(elem: Int): IntList = Cons(elem, this)

  override def contains(elem: Int): Boolean = false

  override def prepend(elem: Int): IntList = new Cons(elem,Empty)

  override def delete(elem: Int): IntList = ???

  override def deleteAll(elem: Int): IntList = ???

  override def prefix(other: IntList): IntList = ???

  /**
    * Returns the number of elements in the list.
    *
    * @return the number of elements in the list
    **/
  override def size: Int = 0
}
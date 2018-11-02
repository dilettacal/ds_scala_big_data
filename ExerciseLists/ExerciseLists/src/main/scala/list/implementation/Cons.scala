package list.implementation

import list.traits.IntList

/**
  * Implements the interface IntList
  * Cons represents the "blocks" in the Linked List
  * Cons: [head | tail] --> [head | tail] --> [ | ] === Empty
  *
  * toString-Method automatically generated --> toString Cons(2,Empty)
  * equals-method --> compares the variables passed in the constructor
  * @param head
  * @param tail
  */
case class Cons(head: Int, tail: IntList) extends SinglyLinkedIntList {
  override def isEmpty = false

  override def get(index: Int): Int = index match {
    case 0 => head
    case i => tail.get(i - 1)
  }

  override def append(elem: Int): IntList = Cons(head, tail.append(elem))

  override def contains(elem: Int): Boolean = ???

  override def prepend(elem: Int): IntList = ???

  override def delete(elem: Int): IntList = ???

  override def deleteAll(elem: Int): IntList = ???
}
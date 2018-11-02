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

  override def contains(elem: Int): Boolean = elem match {
    case y if (y==head) => true
    case _ => tail.contains(elem)
  }

  override def prepend(elem: Int): IntList = new Cons(elem,this)

  override def delete(elem: Int): IntList = this.contains(elem) match {
    case false => this
    case true => null
  }

  override def deleteAll(elem: Int): IntList = ???

  def getIndexOfElem(elem: Int): Int = this.contains(elem) match {
    case false => throw new ArrayIndexOutOfBoundsException
    case true =>

  }
}
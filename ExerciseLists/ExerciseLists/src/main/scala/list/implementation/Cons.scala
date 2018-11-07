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
      //index == 0 --> head, sonst such im tail von vorne bis hinten
    case 0 => head
    case i => tail.get(i - 1)
  }

  //Hinfuegen am Ende
  override def append(elem: Int): IntList = Cons(head, tail.append(elem))

  //Suche beim head, wenn nicht gefunden, wiederhole im tail
  override def contains(elem: Int): Boolean = elem match {
    case _ if elem == head => true
    case _ => tail.contains(elem)
  }

  override def delete(elem: Int): IntList = elem match {
    case _ if elem == head => tail
    case _ => Cons(head,tail.delete(elem))

  }

  override def deleteAll(elem: Int): IntList = elem match {
    case x if x == head => {
      println("X: " + x)
      tail.deleteAll(elem)
    }
    case _ => Cons(head, tail.deleteAll(elem))
  }
  override def prefix(other: IntList): IntList = other match {
    case Empty => this
    //Jedes Element wird der Reihe nach in die neue Liste gespeichert bis
      //other leer ist. In dem Fall wird this angehaengt
    case _ => Cons(other.head, prefix(other.tail))
  }

}
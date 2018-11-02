package list.implementation

import list.traits.IntList

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object SinglyLinkedIntList {


  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): SinglyLinkedIntList = xs match {
    case Seq() => Empty
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => Cons(xs.head, SinglyLinkedIntList(xs.tail: _*))
  }

}

abstract class SinglyLinkedIntList extends IntList {

  override def prefix(other: IntList): IntList = other.isEmpty match {
    case true => this
    case false => new Cons(other.head, this.prefix(other.tail))
  }

  override def size: Int ={
    def count(elem: IntList): Int = {
      elem match{
        case Empty => 0
        case _ => 1 + count(elem.tail) //mit _ funktioniert das nicht!
      }
    }
    count(this)
  }

  //Uebung 4
  override def append(elem:Int): IntList = this match {
    case Empty => Cons(elem,this)
    case Cons(head,tail) => Cons(head, tail.append(elem))
  }

  override def isEmpty: Boolean = (head.equals(null) && tail.equals(null))

  override def get(index: Int) = 0

  /** ------------------------------------------
    *
    * Exercise 5
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = ???

  override def filter(filterFunc: Int => Boolean): IntList = ???

  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = ???

  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = ???

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */

  override def forAll(predicateFunc: Int => Boolean): Boolean = ???

  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = ???

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = ???

  override def insertionSort: IntList = ???

  override def insertSorted(elem: Int): IntList = ???

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = ???
}
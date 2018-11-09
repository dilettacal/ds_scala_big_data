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


  override def size: Int = this match {
    case Empty => 0
    case _ => 1 + this.tail.size
  }

  //Uebung 4
  override def append(elem:Int): IntList = this match {
    case Empty => Cons(elem,this)
    case Cons(head,tail) => Cons(head, tail.append(elem))
  }

  //das kann man sich auch sparen
  override def isEmpty: Boolean = (head.equals(null) && tail.equals(null))

  override def prepend(elem: Int): IntList = Cons(elem, this)

  /** ------------------------------------------
    *
    * Exercise 5
    *
    * ------------------------------------------ */


  override def map(mapFunc: Int => Int): IntList = this.tail match {
      //Wenn tail leer, dann haben wir nur head --> Map func auf head
    case Empty => Cons(mapFunc(this.head), Empty)
     //wenn nicht leer, dann veraendere head und rekursiver Aufruf im tail
    case _ => Cons(mapFunc(this.head), tail.map(mapFunc))
  }

  override def filter(filterFunc: Int => Boolean): IntList = this match {
  //https://www.scala-exercises.org/scala_tutorial/lazy_evaluation
      //Warum???
    case Empty => this
    case _ if(filterFunc(this.head)) => Cons(this.head, this.tail.filter(filterFunc))
    case _ => this.tail.filter(filterFunc)
  }

  //assert(SinglyLinkedIntList(1, 2, 3).foldLeft(5)((x, y) => x + y) === 11)
  //gegeben eine Liste mit Werten und einem Anfangswert, man will die Elemente paarweise aufsummieren + anfangswert
  /*
  Initial 5
head:1
Initial 6
head:2
(E) Initial 8
(E) head:3
   */
  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this.tail match {
    case Empty => {
      println("(E) Initial " +initial)
      println("(E) head:" + head)
      reduceFunc(initial,head)
    }
    case _ => {
      println("Initial " +initial)
      println("head:" + head)
      this.tail.foldLeft(reduceFunc(initial,head))(reduceFunc)
    }
  }


  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this.tail match {
    case Empty => this.head
    case _ => reduceFunc(this.head, this.tail.reduceLeft(reduceFunc))
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */

  //Wendet Predikat predicateFunc auf allen Elementen
  //Returns true, wenn Prädikat für alle gilt
  override def forAll(predicateFunc: Int => Boolean): Boolean = this.tail match {
    //Wenn Liste leer ist, dann auf head
    case Empty => predicateFunc(this.head)
    //Sonst wird predicateFunct auf tail nur dann angewendet, wenn auf head true liefert
    case _ => if(predicateFunc(this.head)) tail.forAll(predicateFunc) else false

  }

  //Wie foldLeft aber mit umgekehrter Reihenfolge von initial und head
  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this.tail match {
    case Empty => {
      println("(E) Initial " +initial)
      println("(E) head:" + this.head)
      reduceFunc(this.head, initial)
    }
    case _ => {
      println("Initial " +initial)
      println("head:" + this.head)
      this.tail.foldRight(reduceFunc(this.head, initial))(reduceFunc)
    }
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this.tail match {
    case Empty => this.head
    case _ => reduceFunc(this.head.reduceRight(reduceFunc), this.tail)
  }

  override def insertionSort: IntList = ???

  override def insertSorted(elem: Int): IntList = ???

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = ???
}
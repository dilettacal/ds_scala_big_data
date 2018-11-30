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
  //override def isEmpty: Boolean = (head.equals(null) && tail.equals(null))

  override def prepend(elem: Int): IntList = Cons(elem, this)

  /** ------------------------------------------
    *
    * Exercise 5
    *
    * ------------------------------------------ */

  override def map(mapFunc: Int => Int): IntList = this match {
    case Empty => Empty
    case Cons(head, tail) => Cons(mapFunc(head), tail.map(mapFunc))
  }


  override def filter(filterFunc: Int => Boolean): IntList = this match {
  //https://www.scala-exercises.org/scala_tutorial/lazy_evaluation
    case Empty => Empty
    case Cons(head, tail) if(filterFunc(this.head)) => Cons(head, tail.filter(filterFunc))
    case Cons(head, tail) => tail.filter(filterFunc)
  }


  override def foldLeft(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons(head,tail)=> tail.foldLeft(reduceFunc(initial,head))(reduceFunc)
  }


  override def reduceLeft(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => throw new IllegalArgumentException("Empty List")
    case Cons(v, Empty) => v
    case Cons(v1, Cons(v2, tail)) => Cons(reduceFunc(v1, v2), tail).reduceLeft(reduceFunc)
  }

  /** ------------------------------------------
    *
    * Assignment 1
    *
    * ------------------------------------------ */

  //Wendet Predikat predicateFunc auf allen Elementen
  //Returns true, wenn Prädikat für alle gilt
  override def forAll(predicateFunc: Int => Boolean): Boolean = this match {
    case Empty => true
   // case Cons(v, Empty) => true
    case _ => if(predicateFunc(head)) tail.forAll(predicateFunc) else false
  }


  override def foldRight(initial: Int)(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => initial
    case Cons(head, tail) => tail.foldRight(reduceFunc(head, initial))(reduceFunc)
  }

  override def reduceRight(reduceFunc: (Int, Int) => Int): Int = this match {
    case Empty => throw new IllegalArgumentException("Empty List")
    case Cons(v, Empty) => v
    case Cons(v1, Cons(v2, tail)) => Cons(reduceFunc(v1,v2), tail).reduceRight(reduceFunc)
  }

  override def insertionSort: IntList = tail match {
    case Empty => this
    case _ => tail.insertionSort.insertSorted(head)
  }

  override def insertSorted(elem: Int): IntList = this match {
    case Empty => Cons(elem, Empty)
    case _  => {
      if(elem >= head)
        Cons(head, tail.insertSorted(elem))
      else
        Cons(elem, this)
    }
  }

  override def foldLeft[A](initial: A)(reduceFunc: (A, Int) => A): A = this match {
    case Empty => initial
    case Cons(head,tail)=> tail.foldLeft(reduceFunc(initial,head))(reduceFunc)
  }

  // Uebung 6

  /**
    * Schreiben Sie eine Funktion reverse, die eine Liste unter Verwendung von foldLeft oder foldRight umdreht
    * @param I
    * @tparam T
    * @return
    */
  def reverse[T](I:List[T]): List[T] = I.foldLeft(List(): List[T])((acc,x) => x::acc)

  /**
    * Schreiben Sie eine FUnktion foldRight unter Verwendung von foldLeft
    * @param base
    * @param I
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A,B](base:B, I:List[A])(f:(A,B)=> B): B = I.reverse.foldLeft(base)((b,a) => f(a,b))


  /**
    * Funktion mapReduce mit der folgenden Implementierung. Berechnen Sie mit dieser Funktion für alle Werte der Eingabeliste
    * die kleinsten Primteiler und addieren Sie diese.
    * @param mapFun
    * @param redFun
    * @param base
    * @param l
    * @tparam S
    * @tparam B
    * @tparam R
    * @return
    */
  def mapReduce[S,B,R](mapFun: S=> B, redFun: (R,B) => R, base:R, l:List[S]):R = {
    l.map(mapFun).foldLeft[R](base)(redFun)
  }

  def findSmallestPrimeDivisor(x:Int, counter:Int): Int = x match {
    case _ if x < 1 => throw new Error("Negative Number or Zero")
    case 1 => 1
    case _ if x % counter == 0 => counter
    case _ => findSmallestPrimeDivisor(x, counter+1)
  }

  //Lösung
  /*def findSmallestPrimDivisorWithMapReduce(x:Int, l:List[Int]) ={
    mapReduce(x => findSmallestPrimeDivisor(x,2), (x:Int, y:Int) => x +y, 0, l)
  }*/

}
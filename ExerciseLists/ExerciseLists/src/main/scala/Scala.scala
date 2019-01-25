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

/*
* Abstrakte Klasse zu einer SinglyLinkedList
*/

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


  def delete(elem:Int): IntList = elem match {
    case y if(y == head) => new Cons(this.tail.head, this.tail.tail)
    case _ => tail.delete(elem)
  }

}

import list.traits.IntList

case object Empty extends SinglyLinkedIntList {

  override def head: Int = throw new IllegalArgumentException("head.nil")

  override def tail: IntList = throw new IllegalArgumentException("tail.nil")

  override def isEmpty = true

  override def get(index: Int) = throw new IndexOutOfBoundsException()

  //Hinfuegen beim Empty --> Empty ist kein Emtpy mehr
  override def append(elem: Int): IntList = new Cons(elem, this)

  override def contains(elem: Int): Boolean = false

  override def delete(elem: Int): IntList = this

  override def deleteAll(elem: Int): IntList = this

  override def prefix(other: IntList): IntList = this
}


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

//Assignment 1 - Problems

import list.implementation.SinglyLinkedIntList
import list.traits.IntList

object Problems {

  /**
    * Given the weight in kilograms, that a bag can hold, and a list of items represented by their weights
    * in grams, calculate how many bags are needed to hold all of the given items.
    *
    * @param capacity   the capacity of a bag in kg
    * @param itemWeight weights of the items in grams
    * @return minimum number of bags required
    */
  def minBagsCount(capacity: Int, itemWeight: IntList): Int = {
    //syntactic sugar itemWeight.reduceRight((value1, value2) => value1+value2).toFloat =>itemWeight.reduceRight((_ + _)).toFloat
    math.ceil(itemWeight.reduceRight((_ + _)).toFloat / capacity).toInt
  }


  /**
    * Given a amount of money and a list of coin values,
    * returns the number of possible ways that the change can be returned using
    * coins of those values.
    *
    * E.x. countChange(4,SinglyLinkedList(1,2)
    * has 3 solutions -> {1+1+1+1 ; 2+2 ; 1+2+1}
    *
    * @param money total amount of change to return
    * @param coins possible coins
    * @return number of possible ways the change can be returned
    */
  def countChange(money: Int, coins: IntList): Int = {
    def change(capacity: Int, changes: IntList): Int = {
      if (capacity == 0) 1
      else if(capacity > 0 && !changes.isEmpty)
        change(capacity - changes.head, changes) + change(capacity, changes.tail)
      else 0
    }
    change(money, coins.insertionSort)
  }


  /**
    * A postman has a list of delivery addresses, for which he and his colleague are responsible.
    * The addresses they have to visit are split into odd and even - one part for each of them.
    * He can choose which part he wants to take. Being lazy, he wants to take the one,
    * where he will be done faster.
    *
    * All addresses are located on a very long street and all odd ones are on one side
    * and the even ones on the other.
    * The distance between 2 addresses can be computed based on their address number.
    *
    * E.x the distance between 40 and 42 ist 1, because they are next to each other
    * (all even numbers on one side, odd on the other).
    * The distance between 40 and 48 is 4.
    * The total distance for the addresses {40, 42, 52} is 6
    *
    * He has to stop at every address and drop off all the mail for this building,
    * which he computes as an extra 2 distance per address.
    * Also taking this into account the distance for the addresses {40, 42, 52} will be 6 + 3x2 = 12
    *
    * The postman starts from one end and goes to the other.
    *
    * Implement the method, which given the addresses predicts which path will be faster.
    *
    * @param addresses the address numbers
    * @return the faster path: true if even, false if odd
    *
    *         //Example of test (45, 47,51, 42, 41, 36, 52, 43)
    *         (41, 43, 45,47,51)
    *         (36,42,52)
    */
  def shouldTakeEvenAddresses(addresses: IntList): Boolean = {
    val sortedAddresses = addresses.insertionSort
    //divide even from odd address numbers
    val allEvenAddrs = sortedAddresses.filter(address => address % 2 == 0)
    val allOddsAddrs = sortedAddresses.filter(address => address % 2 != 0)
    //Stops
    val evenStops = allEvenAddrs.size * 2
    val oddsStops = allOddsAddrs.size * 2
    //Dinstances
    val evenDistance = (allEvenAddrs.get(allEvenAddrs.size - 1) - allEvenAddrs.get(0)).toFloat / 2
    val oddDistance = (allOddsAddrs.get(allOddsAddrs.size - 1) - allOddsAddrs.get(0)).toFloat / 2

    //which is faster?
    //even = pari, odd = dispari!
    evenDistance + evenStops <= oddDistance + oddsStops
  }


  //Solving the following problem is optional, as it is a lot harder than the previous ones.
  // It is not a required part of the assignment, but will make all tests ins ProblemsTest turn green.

  /**
    * The postman has recognized, that addresses which are adjacent to the one
    * in front of which he stops with his delivery bike, take less time and
    * wants to also take this into account.
    *
    * The distance computation for the second condition is modified in the following way:
    * If he stops at 40 the additional distance between 40 and 42 as well as 40 and 38 is 1 instead of 2.
    * The one for 40 is still 2
    *
    * @param addresses the address numbers
    * @return the faster path: true if even, false if odd
    */
  def shouldTakeEvenAddressesExtended(addresses: IntList): Boolean = ???
}


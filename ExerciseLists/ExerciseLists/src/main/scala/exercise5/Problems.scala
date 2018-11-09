package exercise5

import list.implementation
import list.implementation.Empty
import list.traits.IntList


/**
  * Complete the following exercises to practice the usage of higher order functions.
  */
object Problems {

  /**
    * multiplyAndFilterEven should multiply all elements of the IntList by
    * the factor x and filter all element that are even
    */
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = l match {
    case Empty => l
    case _ => l.filter(el => el %2 ==0).map(el => el*x)
  }

  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = ???

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = l match {
    case Empty => 0
    case _ => l.filter(el => el %2 !=0).reduceLeft((x1, x2) => x1+x2)
  }

  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = ???
}

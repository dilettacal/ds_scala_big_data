package exercise5

import list.traits.IntList

/**
  * Complete the following exercises to practice the usage of higher order functions.
  */
object Problems {

  /**
    * multiplyAndFilterEven should multiply all elements of the IntList by
    * the factor x and filter all element that are even
    */
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = l.map(_ * x).filter(_ % 2 == 0)

  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = l.reduceLeft((x, y) => if (y > x) x else y)

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = l.foldLeft(0)((x, y) => if (y % 2 == 0) x else x + y)

  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = l.foldLeft(0)((x, y) => if (y % 2 == 0) x + 1 else x)
}
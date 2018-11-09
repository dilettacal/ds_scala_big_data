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
  def multiplyAndFilterEven(l: IntList, x: Int): IntList = l.filter(value => value %2 ==0).map(value => value*x)

  /**
    * findMin should find the smallest element of a list
    */
  def findMin(l: IntList): Int = l.foldLeft(1)((smallestVal, curVal) => if(curVal < smallestVal) curVal else smallestVal)

  /**
    * sumOddNumbers should sum up all odd numbers of a list
    */
  def sumOddNumbers(l: IntList): Int = l.filter(el => el %2 !=0).reduceLeft((x1, x2) => x1+x2)

  /**
    * countEvenNumbers should count all even numbers of a list
    */
  def countEvenNumbers(l: IntList): Int = l.filter(value => value %2 ==0).foldLeft(0)((counter, curVal) => counter+1)
}

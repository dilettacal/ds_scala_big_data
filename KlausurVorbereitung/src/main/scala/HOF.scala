object HOF extends  App {

  //FoldLeft summe aller Elemente in einer Liste
  /*
  foldLeft function here takes two argument
  1) an initial value of 0 and
  2) a pre-defined combining operation op that
  takes two arguments, i.e. the accumulated
  value acc and the current value i.

  def foldLeft[B](z: B)(op: (B, A) ⇒ B): B

   */
  val inputList: List[Int] = List(1, 3, 5)
  println(inputList.foldLeft(0) { (acc, i) => acc + i })
  println(inputList.foldLeft(0)(_ + _))

  //List lenght
  def len(list: List[Any]): Int = list.foldLeft(0) { (count, _) => count + 1 }
  print(len(inputList))

  //Last elem
  def last[A](list: List[A]): A = list.foldLeft[A](list.head) { (_, cur) => cur }
  println(last(inputList))

  //Average of a list
  def average(list: List[Double]): Double = list match {
    case head :: tail => tail.foldLeft((head, 1.0)) { (avg, cur) =>
      ((avg._1 * avg._2 + cur)/(avg._2 + 1.0), avg._2 + 1.0)
    }._1
    case Nil => -1
  }
  println(average(List(1f,4.5,6,7)))
  println(average(List()))


  //GET item at a given index
  /*def get[A](list: List[A], idx: Int): A =
    list.tail.foldLeft((list.head, 0)) {
      (r, cur) => if (r._2 == idx) r else (c, r._2 + 1)
    } match {
      case (result, index) if (idx == index) => result
      case _ => throw new Exception("Bad index!")
    } */

  //REVERSE
  def reverse[A](list: List[A]): List[A] =
    list.foldLeft(List[A]()) { (r,c) => c :: r }

  /*

  The primary difference among fold, foldLeft and foldRight
  is the order in which the combining operation op
  iterates through the sequence.
  foldLeft starts from the left side (the first value)
  and the values are added to the combining operation
  in left-to-right order;
  foldRight, on the contrary, starts from the right side
   (the last value) and the values are processed
   in right-to-left order. fold in no particular order.
   */

  //01 Function to sum all integers between two given numbers a and b
  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts( a + 1, b)


  //Functions to sum all the integers between two given numbers a and b
  def square(x: Int): Int= x * x
  def sumSqaures(a: Int, b:Int): Int =
    if (a > b) return 0 else square(a) + sumSqaures(a + 1, b)

 /*
 def sum(f: Int => Int, a: Int, b:Int) =
    if (a > b) 0 else f(a) + sum(a + 1 ,b)
 def id(x: Int): Int = x
  def square(x: Int) = x * x
  def sumInts(a: Int, b: Int) = sum(id, a, b)
  def sumSqaures(a; Int, b:Int) = sum(square, a, b)
  */

  /*def sum(f: Int => Int, a: Int, b:Int) =
    if (a > b) 0 else f(a) + sum(a + 1 ,b)
  //Anonymous Function
  (x: Int) => x
  (a: Int) => a * a
  def sumInts(a: Int, b: Int) = sum((x: Int) => x, a, b)
  def sumSquares(a: Int, b:Int) = sum((x: Int) => x * x, a, b)
*/


  //FUnction currying
//Currying allows us to apply some argument to the function now and other at later time when required. The above sum function
// can be curried in the following way
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sumF(a + 1, b)
    sumF
  }



}

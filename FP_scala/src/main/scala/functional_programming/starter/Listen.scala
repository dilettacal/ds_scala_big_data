package functional_programming.starter

object Listen extends App {

  def reverseList(list: List[Int]): List[Int] = list match {
    case x :: xs => {
      //X: 1, XS: List(2,3,5,..)
      //X: 2, XS: remaining...
      //XS ++List(x): List(2, 3, 5, 6, 7, 8, 9, 1)
      println("X: " + x)
      println("XS: " + xs)
      println("XS ++ X: " + (xs++List(x)))
      reverseList(xs) ++ List(x)
    }
    case Nil => Nil
  }

  println("Reversing the list")
  println(reverseList(List(1,2,3,5,6,7,8,9)))
  // Make a list via the companion object factory
  val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  val when = "AM" :: "PM" :: List()
  days match {
    case firstDay :: otherDays =>
      println("The first day of the week is: " + firstDay)
    case List() =>
      println("There don't seem to be any week days.")
  }

  val mainList = List(3, 2, 1)
  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList

  val plusPlus = List(4) ++ mainList
  println(plusPlus)

  println()
  println("Words and counting")
  val words = List("this", "is", "a", "that", "is", "a")
  /**
    * For each word in the words list
    * key <- word
    * if key not in table
    *   insert (key, 1)
    *  else
    *   update count for key to count+1
    */

  println("GroupBy result: ")
  /**
    * (is,List(is, is))
    * (that,List(that))
    * (a,List(a, a))
    * (this,List(this))
    * Groups list element as word, List(word...)
    */
  val groupby = words groupBy(x=>x) foreach println

  println("Map result: ")
  /**
    * (is,2)
    * (that,1)
    * (a,2)
    * (this,1)
    * Groups list element as word, List(word...)
    */
  val mapping = words groupBy(x => x) map { y => (y._1, y._2.length) }
  println(mapping)

  println("Counting frequency result")
  val freq = words groupBy(x => x) map { y => (y._1, y._2.length) } foreach println

}

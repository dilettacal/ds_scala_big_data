package exercises.part2_oop

object Verlag extends App{

  val novel = new Novel("Hamlet", 1967, new Author("William", "Shakespeare", 1564))
  println(novel.authorAge())
  println(novel.isWrittenBy())
  println(novel.copy(2007))
  val counter = new Counter(10)
  println(counter.count)
  println(counter.decrease.count)
  println(counter.increase.count)

  println(counter.decrease(2).count)
}
class Author(firstName: String, surname: String, val year: Int) {
  def fullName(): String = s"${firstName} ${surname}"
}

class Novel (name: String, year: Int, author: Author) {
  def authorAge(): Int = {
    val thisYear: Int = 2018
    thisYear-author.year
  }

  def isWrittenBy(): String = author.fullName()
  def copy(newYear: Int): Novel = new Novel(this.name, newYear, this.author)

}


class Counter(val count: Int){
//  def count = n //the same as constructor with n:Int + method
  def increase = new Counter(count+1)
  def decrease = new Counter(count-1)
  def increase(n: Int): Counter = {
    if(n <= 0) this
    else increase.increase(n-1)
  }

  def decrease(n: Int): Counter = {
    if(n <= 0) this
    else decrease.decrease(n-1)
  }

}
package lectures.part1_basics

object CBNvsCBV extends App{


  def calledByValue(x: Long): Unit = {
    println("by value: " + x)
    println("by value: "+ x)
  }

  //=> parameters are called by name
  def calledByName(x: => Long): Unit = {
    println("by name: " + x)
    println("by name: "+ x)

  }

  calledByValue(System.nanoTime())
  calledByName(System.nanoTime())

  //By value = the value as parameter is computed before the action is performed
  //by name = System.nanoTime() parsed as is. It is used "twice" in both println calls

  def infinite(): Int = 1 + infinite()
  def printFirst(x: Int, y: => Int) = println(x)

  //println(printFirst(infinite(), 32)) // stackoverflow
  println(printFirst(34, infinite())) // infinite() never evaluated because y is not printed out
}

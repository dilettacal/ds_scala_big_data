import java.util

object Intro extends App{
  //Funktionale Konzepte

  //Verdoppelung
  val l:List[Int] = List(5,6,2)
  l.map(x => 2*x).foreach(println) //foreach ist fuer die Anwendung der Funktion. Hat keinen RW

  //Reduzierung von Mengen

  //Reduzierung auf einen Wert
  val listToReduce:List[Int] = List(105,19,24,10,0)
  println(listToReduce.foldLeft(0)(_+_))
  println(listToReduce.foldRight(0)(_+_))

  def square(x:Double) = x*x
  println(square(2))

  def sumOfSquares(x:Double, y:Double):Double = square(x)+square(y)
  println(sumOfSquares(2,4))
  //CBV and CBN
  def first(x:Int, y:Int) = x
  def loop:Int = loop

  //CBN
 // println(first(5,loop)) //ewige Loop :-)

  //Evaluation strategy with =>
  def constOne(x:Int, y: => Int) = 1
  println(constOne(5, loop)) //1
  //println(constOne(loop, 4)) //ewige Loop
}


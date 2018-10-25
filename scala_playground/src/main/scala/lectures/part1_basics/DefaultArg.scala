package lectures.part1_basics

object DefaultArg extends App {

  def trFact(n: Int, acc:Int = 1):Int = {
    if (n<= 1) acc
    else trFact(n-1, n*acc)
  }

  val fact10 = trFact(10)
  println(fact10)

  def savePicture(format: String, width: Int = 20, height: Int = 20): Unit = println("Saving picture")
  savePicture("jpeg")


}

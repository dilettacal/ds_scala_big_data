object Intro2  extends  App {

  def loop:Boolean = loop
  def and(x:Boolean, y:Boolean): Boolean = if(x) y else false

  //println(and(false,loop)) //ewig!

  def and2(x:Boolean, y: => Boolean) = if(x) y else false

  println(and2(false, loop)) //false --> mit true ist das ein ewiges for Loop

  val x = 0
  def f(y: Int) = y+1
    val result  = {
    val x = f(3)
    x*x
  } + x

  println(result)


}

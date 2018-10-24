package lectures.part1_basics

object Expressions extends App {

  val x: Int = 1+2 //EXPRESSION

  println(1 == x)
  println(!(1==x))

  var aVariable = 2
  aVariable +=6 //Side effects
  println(aVariable)

  //Instructions (DO) vs expressions (VALUE) - Expression in scala are evaluated, in java executed

  // IF expression
  val aCondition = true
  val aConditionedValue = if(aCondition) 5 else 3
  println(aConditionedValue)
  println(if(aCondition) 5 else 3)

  //Avoid while loops :-)

  //SIDE EFFECTS
  var i = 0
  val aWhile = while (i < 10){
    //println(i)
    i += 1
  }

  println(aWhile) //()
  //everything is an expression
  val aWeirdValue = (aVariable = 3) //Type Unit == void
  print(aWeirdValue) //()

  //Side effects: println(), whiles, reassigning --> Imperative programming

  //CODE BLOCKS
  //code blocks are expressions and have the value of the last expression (here: String)
  val aCodeBlock: String = {
    val y = 2
    val z = y+1
    if(z>2) "hello" else "good bye" //Define the type
  }

  //val anotherValue = z +1 //z not visible outside the block






}

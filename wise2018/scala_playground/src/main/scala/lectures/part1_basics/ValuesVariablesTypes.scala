package lectures.part1_basics

object ValuesVariablesTypes extends App {
  val x: Int = 42
  println(x)
  //x = 2 Vals cannot be reassigned (final)
  //VALS are immutable

  val x1 = 42 //Type can be omitted because the compiler can infer types
  println(x1)

  //Datatypes and val
  val aString: String = "Hello, I am a string :-)"  //Semicolons only to separate expression
  val aBoolean: Boolean = true
  val aChar: Char = 'h'
  val anInt: Int = x
  val aShort: Short = 4613
  val aLong: Long = 458258851529593L
  val aDouble: Double = 3.14
  val aFloat: Float = 1.2f

  //VARIABLES - mutable objects
  var aVariable: Int = 4
  aVariable = 5 //side effects

}

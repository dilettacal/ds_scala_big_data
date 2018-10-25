package lectures.part1_basics

object StringOperations extends App {

  val str: String = "String :-)"

  //S-Interpolators
  val name = "David"
  val age = 12
  val greeting = s"Hello, my name is $name and I am $age years old"
  val anotherGreeting = s"Hello, my name is $name and I will be turning ${age+1} years old"
  println(greeting)
  println(anotherGreeting)

  //F-Interpolators == printf
  val speed = 1.2f
  //f == interpolated formatted string
  val myth = f"$name%s can eat $speed%2.4f burgers per minute"
  println(myth)


  //raw-interpolators
  println(raw"This is a \n newline") //literally printed!
  println(s"This is a \nnewline")
  val escaped = "This is a \n newline" //injected variable with escaped sign are parsed
  println(raw"$escaped")

}

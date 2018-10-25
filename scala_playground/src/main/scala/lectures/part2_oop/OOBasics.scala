package lectures.part2_oop

object OOBasics extends App {
  val person = new Person("Robert", 22)
  person.greet("Robert")
  person.greetThis("Luke")
  person.greet()

  val person2 = new Person("Kim")
  person2.greet()
  println(person2.x)

}
//constructor
//to access class parameters from outside: val keyword
class Person(name: String, val age: Int) {
  //Class parameters are not fields and cannot be accessed from outside
  //IN order to access a class parameter, this should be declared as val
  val x = 2
  //name is the parameter
  def greet(name: String): Unit = println(s"$name says: Hi, $name")
  def greetThis(name: String): Unit = println(s"${this.name} says: Hi, $name")

  //method overloading
  def greet(): Unit = println(s"Hi, I am $name")

  //Multiple constructors
  def this(name: String) = this(name,0)
  def this() = this("John Doe")
}


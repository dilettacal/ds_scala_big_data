package exercises.part2_oop

object MethodNotations extends App {

  class Person(val name: String, favoriteMovie: String){
    def this(age: Int) = this(name,favoriteMovie, age)
    def likes(movie: String): Boolean = movie==favoriteMovie
    def +(person:Person): String = s"${this.name} is hanging out with ${person.name}"
    //overload
    def +(nickname: String) : Person = new Person(s"${this.name} (the $nickname)", this.favoriteMovie)
    def hangOutWith(person: Person): String = s"${this.name} is hanging out ${person.name}"
    def unary_! : String = s"$name, what the hell?"
    def isAlive: Boolean = true
    def apply(): String = s"Hi, my name is $name and I like $favoriteMovie"
  }

  val mary = new Person("Mary", "Inception")

  println(mary.likes("Inception"))
  println(mary likes "Inception") //equivalent form
  println(mary likes("Inception")) //equivalent form
  //infix notation = operator notation --> Methods only with 1 parameter!!!

  //"operators" in Scala
  val tom = new Person("Tom", "Fight club")
  println(mary hangOutWith tom) //infix style - single parameter
  println(mary + tom)
  println(mary.+(tom))

  //prefix notation
  val x = -1
  val y = 1.unary_-
  val z = 1.unary_+

  println(!mary)
  println(mary isAlive)

  //apply
  println(mary.apply())
  println(mary()) //equivalent
  //objects called like a function --> compiler search for apply method

  println(mary.+("rockstar").name)

}

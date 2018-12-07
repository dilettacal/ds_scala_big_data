object GenericList extends App {


  class MyList[+A]{
    // use the type A

    //B is supertype of A (>:)
    def add[B >: A](element: B): MyList[B] = ???

    /*

      A = Cat

      B = Animal

     */
  }

  val listOfIntegers = new MyList[Int]

  //companion for MyList
  object MyList{
    def empty[A]: MyList[A] = ???

  }

  val emptyListOfIntegers = MyList.empty[Int] //returns a MyList of Integers


  // variance problem

  class Animal
  class Cat extends Animal
  class Dog extends Animal

  //Does a List[Cat] extend the List[Animal]
  // 1. yes, List[Cat] extends List[Animal] = COVARIANCE
  class CovariantList[+A] ///+ == covariant list
  val animal: Animal = new Cat
  val animalList: CovariantList[Animal] = new CovariantList[Cat]

  // animalList.add(new Dog) ??? HARD QUESTION => we return a list of Animals


  // 2. NO = INVARIANCE
  class InvariantList[A]
  //invariant classes: No substitution
  val invariantAnimalList: InvariantList[Animal] = new InvariantList[Animal]
 // val test: InvariantList[Animal] = new InvariantList[Cat]

  // 3. Hell, no! CONTRAVARIANCE
  class Contravariant[-A]
  val contravariantList: Contravariant[Cat] = new Contravariant[Animal]

  class Trainer[-A]
  val trainer: Trainer[Cat] = new Trainer[Animal]


  // bounded types

  class Cage[A <: Animal](animal: A)
  val cage = new Cage(new Dog)


  class Car

  // generic type needs proper bounded type

  //  val newCage = new Cage(new Car)
}

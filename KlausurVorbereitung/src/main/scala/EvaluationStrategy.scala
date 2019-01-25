object EvaluationStrategy {
  def main(args: Array[String]): Unit = {

    /**
      * Call by value, all function arguments are evaluated once, and instant
      */
    callByValue(myfun())

    /**
      * Call by name, the function is only executed n case we want to access the string variable, which is never
      */
    callByName(myfun())

    //This is executed when object is created
    val myVar = {
      println("Uhhh, I'm instantly evaluted")
      1
    }

    //Functions are executed on demand
    def myFun: Int = {
      println("Not executed ...")
      1
    }
  }


  def callByValue(f: String): Unit = {
    println("Call by value")
    //Nothing :)
  }

  def callByName(f: => String): Unit = {
    println("Call by name")
    //Nothing :)
    println("Sth::" + f)
    println("Sth:" + f)
  }

  def myfun(): String = {
    println("f is evaluated")
    "hello"
  }
}
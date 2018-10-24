package lectures.part1_basics

object Functions extends App {

  //Normal functions do not need return types
  def aFunction(a: String, b: Int)=  a + " " + b

  def aParameterLessFunction(): Int = 32
  println(aParameterLessFunction())
  println(aParameterLessFunction)

  //NO LOOP --> RECURSION
  //Recursive functions need a return type
  def aRepeatedFunction(aString: String, n:Int): String = {
    if(n==1) aString
    else aString+aRepeatedFunction(aString, n-1)
  }
  println(aRepeatedFunction("Hello", 3))

  def aFunctionWithSideEffects(aString: String): Unit = println(aString)

  print(aFunctionWithSideEffects("Side effects?"))

  //INNER FUNCTION
  def aBigFunction(n: Int): Int = {
    def aSmallerFunction(a: Int, b:Int): Int = a+b
    aSmallerFunction(n, n-1)
  }

  println(aBigFunction(6))

  //1. A greeting function (name, age)
  //2. Factorial function 1* 2 * 3 ... * n
  //3. A Fibonacci function
  //f(1) = 1, f(2) = 1, f(n) = f(n-1)+f(n-2)
  //4. test if a number is prime

  def greeting(aName: String, anAge: Int): String = "Hi, my name is " + aName + " and I am " + anAge + " years old"

  def factorial(n: Int): Int = {
    if (n <= 0) 1 else n*factorial(n-1)
  }

  def fibonacci(n: Int): Int =
    if (n <=2) 1
    else fibonacci(n-1)+fibonacci(n-2)

  println(greeting("Marco", 22))
  println(factorial(5))
  println(fibonacci(8))

  def isPrime(n: Int): Boolean = {
    //does n have any divisors until number t?
    def isPrimeUntil(t: Int): Boolean = {
      if(t <= 1) true
      else n % t != 0 && isPrimeUntil(t -1)
    }
    isPrimeUntil(n/2)
  }

  println(isPrime(11))

}

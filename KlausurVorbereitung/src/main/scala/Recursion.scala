/**
  * 1. Find a termination condition / a base case
  * 2. Expressing the base case as the first rule
  * 3. Finding the recursion step
  * 4. Express the particular rules with respect to the required order
  */

object Recursion extends  App {

  //Linear Recursion = One single recursive call + calculation runs along a chain of calls
  def sum(n:Int):Int = {
    if(n==0) 0
    else sum(n-1)+n
  }
  println(sum(100))

  def sumUpTo(x:Int, max:Int): Int = {
    if(x>max) 0
    else x+sumUpTo(x+1, max)
  }

  println(sumUpTo(5,20))

  //Sum all multiples of 3 and 5 for a given value

  def sumOfMultiples(value: Integer):Integer = {
    if(value<0) 0
    else if((value % 3 ==0) || (value % 5 == 0)){
      println("value: " + value)
      value + sumOfMultiples(value-1)
    }
    //keep searching
    else sumOfMultiples(value-1)
  }
  println(sumOfMultiples(10))

  //Calculation of the 10001 prime number

  //What is a prime number?
  //A whole number which can only be divided by one or itself

  def is_prim(x:Int) = checkNumber(x,2, math.sqrt(x).toInt+1)
  def checkNumber(x:Int, i:Int, max:Int):Boolean = {
    if(i > max) true
    else if(x % i == 0) false
    else checkNumber(x, i+1,max)
  }

  println(is_prim(3))

  def multiple2(x:Double):Double = x match {
    case 0 => 0
    case y if((y%3==0)||(y%5 ==0)) => (y + multiple2(y-1))
    case _ => throw new Error("No negative Numbers!")
  }

  //Visibility
  val x = 0
  def f(y:Int) = y+1
  val result = {
    val x = f(3) //this x is not the same as above!
    x*x
  }

  println(result)
  println(x)
}

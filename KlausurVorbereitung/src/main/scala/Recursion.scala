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
  println(sumOfMultiples(10))//33
    /*
    value: 10
  value: 9
  value: 6
  value: 5
  value: 3
  value: 0
     */

  //Calculation of the 10001 prime number

  //What is a prime number?
  //A whole number which can only be divided by one or itself
  //This function calls is_prim until the wanted number is found

  def is_prim(x:Int) = checkNumber(x,2, math.sqrt(x).toInt+1)
  def checkNumber(x:Int, i:Int, max:Int):Boolean = {
    if(i > max) true
    else if(x % i == 0) false
    else checkNumber(x, i+1,max)
  }

  println(is_prim(3)) //true
  println(is_prim(10)) //false

  def primNr(X:Int)= getPrim(X,2)
  def getPrim(nr:Int, currentNr:Int):Int =
    if(nr <= 0) currentNr-1
    else if(is_prim(currentNr)) {
      println("currentNr is prime")
      println("Value of currentNr: " + currentNr)
      println("Value of nr: " + nr)
      getPrim(nr-1, currentNr+1)
    }
    else {
      println("CurrentNr is not prime")
      println("Value of currentNr: " + currentNr)
      println("Value of nr: " + nr)
      getPrim(nr, currentNr+1)
    }


//  println("primNr: " + primNr(3)) //7
  println("isPrime: " + is_prim(10)) //false
  println("primNr result: " + primNr(10)) //31

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


  //EXample Parenthesis balanced
  def balance(chars: List[Char]): Boolean = {
    //check if parantheses are balanced
    def isParanthesesbalanced(chars: List[Char], numberOfOpens: Int): Boolean = {
      if (chars.isEmpty) numberOfOpens == 0
      else if (chars.head == '(')
        isParanthesesbalanced(chars.tail,numberOfOpens+1)
      else if (chars.head == ')')
        numberOfOpens>0 && isParanthesesbalanced(chars.tail,numberOfOpens-1)
      else isParanthesesbalanced(chars.tail,numberOfOpens)
    }
    //call function
    isParanthesesbalanced(chars,0)
  }

  println("balance: '(if (zero? x) max (/ 1 x))' is balanced: "+balance("(if (zero? x) max (/ 1 x))".toList)) //true
  println("balance: 'I told him ...' is balanced: "+balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)) //true
  println("balance: ':-)' is unbalanced: "+balance(":-)".toList)) //false
  println("balance: Expression '())(' is balanced:  "+balance("())(".toList)) //false
}

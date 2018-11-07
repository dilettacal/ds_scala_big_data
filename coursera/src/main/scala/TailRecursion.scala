import scala.annotation.tailrec

object TailRecursion extends App {

  @tailrec
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  println(gcd(14,21))
  //Evaluation
  /**
    * if (21==0) 14 else gcd(21, 14%21) //14
    * if(false) 14 else gcd(21,14%21)
    * gcd(21,14%21)
    * gcd(21,14)
    * if(14==0) 21 else gcd(14, 21%14) //7
    * gcd(14,7)
    * if(7==0) 14 else gcd(7, 14%7)
    * gcd(7,0)
    * if(0 == 0) 7
    * 7
    */

  //Not tail recursive
  //After factorial(n-1) there is a multiplication
  def factorial(n:Int): Int = {
    if(n==0) 1 else n*factorial(n-1)
  }
  println(factorial(11))

  def factorialTailRec(n:Int): Int =
  {
    def loop(acc: Int, n: Int): Int =
    {
      if(n==0) acc
      else loop(acc*n, n-1)
    }
    loop(1,n)
  }

  println(factorialTailRec(4))


}

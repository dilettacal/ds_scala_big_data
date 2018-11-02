package uebung3

import scala.annotation.tailrec

object Uebung3 extends App {

  def digitSum(x: Int): Int =
    //modulo berechnet Rest
    //division 'teilt' die Zahl auf
    x match {
      case 0 => 0
      case _ => x % 10 + digitSum(x / 10)
    }


  def findMin(ints: Array[Int]): Int = {
    println("Find minimum in Array:")
    if (ints.isEmpty) -1
    else{
      var min = ints(0);
      for ( i <- 1 to (ints.length - 1) ) {
        if (ints(i) < min) min = ints(i);
      }
      min
    }
  }
  def findMinOpt(ints: Array[Int]): Int = {
    println("Find minimum in Array (Opt):")
    if(ints.isEmpty) -1
    else ints.min
  }

  def fibo(n:Int):Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibo(n-1)+fibo(n-2)
    }
  }

  //https://medium.com/@frank.tan/fibonacci-tail-recursive-explained-876edf5e86fc
  def fiboTailRec(n: Int) = {
    //only the previous two elements are needed
    @tailrec
    def fibo(n: Int, prev: Int, current:Int): Int = {
     // println("Current value of n: " + n)
     // println("Current value of current: " + current)
      // println("Current value of prev: " + prev)
      if(n <= 0) current //end reached
      else fibo(n-1, prev = prev+current, current=prev)
    }

    fibo(n, prev=1, current = 0)
  }

  //Beispiel aus Uebung
  //n^m
  def pow(n: Int, m: Int): Int = m match {
    case 0 => 1
    case 1 => n
    case _ => n * pow(n, m - 1)
  }

  //var res = 1
  //for(i = 0; i< m; i+=1){
  // res*m

  def powOptimiert(n: Int, m: Int) ={
    @tailrec
    def pow(n: Int, m: Int, res: Int): Int = {
      m match {
        case 0 => 1
        case 1 => n * res
        case _ => pow(n, m - 1, res * m)
      }
    }
    pow(n,m, 1)
  }

  //palyndrom: hannah (6 Zeichen)
  def isPalyndrom(str: String): Boolean = {
    val even = str.length%2==0
    even match{
      case true => str.substring(0,str.length/2).equalsIgnoreCase(str.substring(str.length/2, str.length).reverse)
      case _ => str.substring(0,str.length/2).equalsIgnoreCase(str.substring(str.length/2+1, str.length).reverse)
    }
  }

  def isPalyndromProcedural(str: String) = str.equalsIgnoreCase(str.reverse)

  //http://blog.thedigitalcatonline.com/blog/2015/04/07/99-scala-problems-06-palindome/
  def isPalyndromRec(str: String) = true
  def isPalyndromTailRec(str: String) = true

  println("Digit sum: ")
  println(digitSum(5320))
  var myList = Array(1, 2, 5, -1)
  println("Find minimum: ")
  println(findMinOpt(myList))
  println("Power method: ")
  println(pow(2,3))
  println(powOptimiert(3,2))
  println("Fibonacci normal recursiv")
  println(fibo(10))
  println("Fibonacci tail rec")
  println(fiboTailRec(10))

  println(isPalyndromProcedural("hannah"))
  println(isPalyndromProcedural("lagertonnennotregal"))
  println(isPalyndromProcedural("anna"))
  println(isPalyndromProcedural("otto"))
  println(isPalyndromProcedural("bob"))
  println(isPalyndromProcedural("Robert"))

}

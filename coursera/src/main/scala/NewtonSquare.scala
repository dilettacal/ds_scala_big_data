object NewtonSquare extends App {



  /* Calculates the square root of parameter x
    * Successive approximation by Newton.
    * */
  def sqrt(x: Double): Double = {
    def isGoodEnough(estimation: Double):Boolean = {
      abs(estimation*estimation - x) / x < 0.001
    }
    def improve(d: Double): Double ={
      (d + x / d)/2
    }
    def abs(x:Double): Double = if(x>0) x else -x
    def sqrtIter(estimation: Double): Double ={
      if(isGoodEnough(estimation)) estimation
      else sqrtIter(improve(estimation))
    }
    sqrtIter(1)
  }

  println(sqrt(4))
  println(sqrt(1e-6)) //not precise for very small numbers
  print(sqrt(1e60)) //not terminating!
}

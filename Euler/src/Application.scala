
/**
 * Implementation of Euler problem 5
 * Euler project
 * https://projecteuler.net/problem=5
 */
object Application {
  
  def main(args: Array[String]): Unit = {
    println(euler) 
    println(eulerOpt)
  }
  
  def euler: Int = {
   var i = 1;
 
    while   (i %  2 != 0 || i %  3 != 0 || i %  4 != 0 || i %  5 != 0 ||
             i %  6 != 0 || i %  7 != 0 || i %  8 != 0 || i %  9 != 0 ||
             i % 10 != 0 || i % 11 != 0 || i % 12 != 0 || i % 13 != 0 ||
             i % 14 != 0 || i % 15 != 0 || i % 16 != 0 || i % 17 != 0 ||
             i % 18 != 0 || i % 19 != 0 || i % 20 != 0 ){
        i = i+1
    }
    return i;
    
  }
  
  def eulerOpt: Int = {
    var x = 20
    while((2 to 20).forall(x %_ ==0) == false){
      x += 20
    }
    return x
  }
  
}
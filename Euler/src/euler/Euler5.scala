

package euler


/**
 * Implementation of Euler problem 5
 * Euler project
 * https://projecteuler.net/problem=5
 */
class Euler5 {
  
  /**
   * Variante mit Primfaktorzerlegung fuer 1->10
   * 2520
   * 1 2 3 4 5 6 7 8 9 10
   * 
   * 1. Primfaktorzerlegung
   * 2. Es bleiben folgende Zahlen uebrig:
   * 1, 2^3, 3^2, 5, 7
   * 8*9*5*7 = 40*9*7 = 2520 
   * 
   */
  
  
  def euler20: Int = {
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
  
  def euler20Opt: Int = {
    var x = 20
    while((2 to 20).forall(x %_ ==0) == false){
      x += 20
    }
    return x
  }
  


  /**def bruteForce(n: Int): Int {
    var current = 0
    var isSolved = false
    while(!isSolved)={
      current+=1
      isSolved=true
      for(i <-1 to n){
        if(current % i != 0)
          isSolved = false
      }
    }
   return current
  }*/
  
  
  def bruteForceFunctional(n: Int): Int = {
    val range = (1 to n).toArray
    //(1 to Int.MaxValue).par.find(x => range.forall(i => x%i==0)).get
    (1 to Int.MaxValue).find(x => range.forall(i => x%i==0)).get
  }
}
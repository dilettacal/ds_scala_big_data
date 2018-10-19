package exercise1

class Rational(numerator: Int, denominator: Int) {

  def this(denom: Int) = this(1, denom)

  override def toString: String = numerator + "/" + denominator

  require(denominator != 0, "Denominator muss != 0 sein") // throws a IllegalArgumentException if the condition is not met
  println("Eine Rationale Zahl wurde erzeugt....") // is part of the constructor

  def num: Int = numerator // so the numerator is accessible from other classes
  def denom: Int = denominator // so the numerator is accessible from other classes
  def value: Double = num.toDouble / denom // converts to floating point number

  def max(other: Rational): Rational = {
    if (numerator / denominator < other.num / other.denom) this else other
  }

  /*
   * a/b * c/d = a*c/b*d
   */
  def mul(other:Int): Rational = new Rational(other*numerator,other*denominator)
  
  /*
   * a/b + c/d = (a*d + c*b)/(b*d)
   */
  def add(other: Rational): Rational = new Rational(numerator*other.denom + other.num*denominator, denominator*other.denom)
  
  /*
   * a/b + c/d = (a*d + c*b)/(b*d)
   */
  def neg: Rational = new Rational(numerator *(-1), denominator)
  
  /*
   * a/b - c/d = a/b +(neg(c/d))
   */
  def sub(other: Rational): Rational = add(other.neg) //add(neg(other))
  
  //def sub(other:Rational): Rational = new Rational(numerator*other.denom - other.num*denominator, denominator*other.denom)
  
  /*
   * Using symbolic definitions
   */
  
  //def *(other: Rational): mul(other)
  //def +(other:Rational): add(other)
}

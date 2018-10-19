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

  def mul(x:Int): Rational = new Rational(x*numerator,x*denominator)
  /*
   * 1/3 + 1/2 = 3*1 + 2*1 /(2*3)
   */
  def add(r: Rational): Rational = new Rational(numerator*r.denom + r.num*denominator, denominator*r.denom)
  def neg: Rational = new Rational(numerator *(-1), denominator)
  def sub(r: Rational): Rational = add(r.neg)
  
}

package uebung1

class Rational (numerator:Int, denominator:Int){
  //val numerator:Int, val denominator:Int macht die Paramter public
  
  def this (denom:Int) = this(1,denom) //ruft anderen Konstruktor mit Zaehler=1 auf 
  override def toString:String = numerator + "/" + denominator
  
  require (denominator!=0,"Denominator mmuss != 0 sein")  // wirft IllegalArgumentException
  println("Eine Rationale Zahl wurde erzeugt....")  // ist Teil des Konstruktors

  def num:Int=numerator  // damit numerator von aussen zugaenglich ist
  def denom:Int=denominator // damit denominator von aussen zugaenglich ist
  def value:Double = (num.toDouble / denom) // Konvertierung in eine Fliesskommazahl
  
  def  max(x:Rational): Rational = {
    
    if (numerator/denominator<x.num/x.denom) this else x
  }
  def mul(x:Int): Rational = new Rational(x*numerator,x*denominator)
  def add(r: Rational): Rational = new Rational(numerator*r.denom + r.num*denominator, denominator*r.denom)
  def neg: Rational = new Rational(numerator *(-1), denominator)
  def sub(r: Rational): Rational = add(r.neg)
    //r.neg.add(this)
  //this.add(r.neg)
  
}

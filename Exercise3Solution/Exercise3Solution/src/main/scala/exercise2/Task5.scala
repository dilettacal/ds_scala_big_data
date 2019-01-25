package exercise2

object Task5 extends App {

  def isPalindrome(s: String): Boolean = s match {
    case _ if s.isEmpty => true
    case _ if s.length == 1 => true
    case _ if s.head != s.last => false
    case _ => isPalindrome(s.substring(1, s.length - 1))
  }

  println(isPalindrome(""))
  println(isPalindrome("TeseT"))
  println(isPalindrome("Test"))
}

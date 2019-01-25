package exercise2

object Task6 extends App {

  def nMissingParenthesis(s: String): Int = {
    def inner(n: Int, s: String): Int = s match {
      case _ if s.isEmpty => n
      case _ if s.head == '{' => inner(n + 1, s.tail)
      case _ if s.last == '}' => inner(n - 1, s.tail)
    }

    inner(0, s)
  }

  println(nMissingParenthesis("{}"))
  println(nMissingParenthesis("{{{}}"))
  println(nMissingParenthesis("{{}}{}}"))
}

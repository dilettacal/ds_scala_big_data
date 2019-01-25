object Asd extends App {

  val test = (115195195291L).toString.view.map(_.asDigit).sum
  println(test)

  println(List(2,4,5)::List(2,4,5,6)) //List(List(2, 4, 5), 2, 4, 5, 6)
  println(List(2,4,5):::List(2,4,5,6)) //List(2, 4, 5, 2, 4, 5, 6)

  (x: Int) => x
}

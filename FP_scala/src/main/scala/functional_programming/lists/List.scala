package functional_programming.lists

object List extends  App{

    sealed trait List[+A] //A makes the list polymorphic, + makes list covariant

    case object Nil extends List[Nothing]
    //case class ::[+A]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]


    //Constructing a list
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))


    def head[A](list: List[A]): A = list match {
      case Nil => sys.error("tail of empty list")
      //case x :: _ => x or x Cons _ => x
      case Cons(x, _) => x
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => sys.error("head of empty list")
      //case _ :: xs => xs or case _ Cons xs => xs
      case Cons(_, x) => x
    }

  //Elimina tutti gli elementi da indice n a indice 0
    def drop[A](l: List[A], n:Int): List[A] = {
      if (n<=0) l
      else l match {
        case Nil => Nil
        case _ Cons t => drop(t, n-1)
      }
    }

    def dropWhile[A](l: List[A], f: A=>Boolean): List[A]  = l match {
      case x Cons xs if f(x) => dropWhile(xs, f)
      case _ => l
    }


  val list = List(1,2,3,4)
  println(tail(list))
  println(head(list))
  println(drop(list,3))
}




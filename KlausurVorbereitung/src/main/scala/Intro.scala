import java.util

object Intro extends App{
  //Funktionale Konzepte

  //Verdoppelung
  val l:List[Int] = List(5,6,2)
  l.map(x => 2*x).foreach(println) //foreach ist fuer die Anwendung der Funktion. Hat keinen RW
}


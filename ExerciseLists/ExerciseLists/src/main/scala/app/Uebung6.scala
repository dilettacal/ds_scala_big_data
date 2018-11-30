package app

object Uebung6 extends App {

  /**
    * Gegeben sei die folgenden Liste, die ausdrueckt,
    * welche Programmiersprachen welche Paradigmen unterstützen
    */

  val Paradigmen = List(
    ("erlang", "funktional"),
    ("erlang", "logisch"),
    ("prolog", "logisch"),
    ("scala", "funktional"),
    ("scala", "objektorientiert"),
    ("scala", "logisch"),
    ("java", "objektorientiert")
  )

  /*
  Ermitteln Sie unter Verwendung von foldLeft oder foldRight welchs Paradigma wie haeufig in der Liste vorkommt
   */

  val absolute = Paradigmen.foldLeft(Map.empty[String, Int])((acc,x) => acc.updated(x._2, acc.getOrElse(x._2, 0)+1))
  println("Mapping der Werte: " + absolute)

  /**
    * Berechnen Sie aus dem Ergebnis von a die relative Haeufigkeit mit der ein Paradigma vorkommt
    */

  val relative = absolute.mapValues(x=> x.toDouble/Paradigmen.size)
  println("Relative Haeugigkeit: " +relative)


  /**
    * Schreiben Sie eine Funktion partial
    * @param a, ein Wert, mit dem die Funktion belegt werden soll
    * @param f, eine Funktion mit 2 Variablen
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    *
    * Die Funktion soll eine Funktion zurueckliefern, bei der der erste Parameter bereits belegt wurde
    *
    * Beispiel: ​partial(​1​, (a:Int,b:Int)=>a+b)​
    * soll eine Funktion zurückliefern, die einen Wert um 1 erhöht
    */
  def partial[A,B,C](a:A, f:(A,B) => C): B => C = b => f(a,b)

  val f = partial(1, (a:Int, b: Int) => a+b)

  println(f(1))
  println(f(2))

}

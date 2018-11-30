package app

object Uebung6_ extends App {

  /**
    * Schreiben Sie eine Funktion reverse, die eine Liste unter Verwendung von foldLeft oder foldRight umdreht
    * @param I
    * @tparam T
    * @return
    */
  //wenn man x::acc macht, dann wird die Liste von der tail aufgebaut:
  //4, 3, 2, 1 --> Am Ende werden sie in der richtigen Reihenfolge zurueckgegeben
  //:: == prepend (am Anfang hinzufügen)
  //:+ == append (am Ende)
  def reverse[T](l:List[T]): List[T] = l.foldRight(List.empty[T])
  {
    (x,acc) => println(x); println(acc);acc:+x
  }

  def reverseLeft[T](I:List[T]): List[T] = I.foldLeft(List(): List[T])((acc,x) => x::acc)
  println(reverseLeft(List(1,2,3,4)))

  println(List(1,2,3,4):+5)
  println(5:: List(1,2,3,4))
  println(List(1,2,3,4) ++ List(2,3,4,5))

  /**
    * Schreiben Sie eine FUnktion foldRight unter Verwendung von foldLeft
    * @param base
    * @param I
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A,B](base:B, I:List[A])(f:(A,B)=> B): B = ???

  /**
    * Funktion mapReduce mit der folgenden Implementierung. Berechnen Sie mit dieser Funktion für alle Werte der Eingabeliste
    * die kleinsten Primteiler und addieren Sie diese.
    * @param mapFun
    * @param redFun
    * @param base
    * @param l
    * @tparam S
    * @tparam B
    * @tparam R
    * @return
    */
  def mapReduce[S,B,R](mapFun: S=> B, redFun: (R,B) => R, base:R, l:List[S]):R = {
    l.map(mapFun).foldLeft[R](base)(redFun) }

  def findSmallestPrimeDivisor(x:Int, counter:Int): Int = x match {
    case _ if x < 1 => throw new Error("Negative Number or Zero")
    case 1 => 1
    case _ if x % counter == 0 => counter
    case _ => findSmallestPrimeDivisor(x, counter+1)
  }


  /**
    * Beleg3 relevant
    * Gegeben sei die folgenden Liste, die ausdrueckt,
    * welche Programmiersprachen welche Paradigmen unterstützen
    */

  val paradigmen = List(
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

  //Leere Map für keys und Values
  //bei jedem schritt: ist wert schon in der Map? Wenn ja, 0 und um 1 inkrementieren
  //updated --> gibt eine neue Map zurück
  val absolute = paradigmen.foldLeft(Map.empty[String, Int])((acc,x) => acc.updated(x._2, acc.getOrElse(x._2, 0)+1))
  println("Mapping der Werte: " + absolute)


  /**
    * Berechnen Sie aus dem Ergebnis von a die relative Haeufigkeit mit der ein Paradigma vorkommt
    */

  val relative = absolute.mapValues(x=> x.toDouble/paradigmen.size)
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


  /// Beispiele

  val m1 = Map("logisch" -> 1)
  val m2 = m1.updated("funktional", 2)
  println(m1)
  println(m2)
  println(m2.getOrElse("aaa", 5))
  val l1 = m2.toList
  println(l1)
  println(l1.map(x=> x._2))
  //verbose
  println(l1.map{case(_,count) => count})

  println(m2.mapValues(x => x+1))
  val absolute2 = m1.updated("funktional", 2)
  val s = absolute2.values.sum
  println(s)
  val relat = absolute2.mapValues(x => x.toDouble/s)

  println(relat)

  //MapReduce wörter zählen
  /**
    * 1. (erlang, funkt) -> X
    * //Map[String, Int]
    * //List(String, Int)
    * 2. X, A -> A (reduce)
    *
    * Interessant hier ist nur der zweite Wert.
    */
  val mRMap = mapReduce[(String, String),String,Map[String, Int]](
    entry => entry._2,
    //(acc, x_2)
    (map, paradigm) =>
      map.updated(paradigm, map.getOrElse(paradigm, 0) + 1), //map nimmt zunächst Base-Element, also Leere neue Liste
    Map.empty[String, Int],
    paradigmen
  ).toSet

  println(mRMap)

  //Mengenoperatoren

  val a = Set(1,2,3)
  val b = Set(2,3,4)

  println( a union b)
  println(a intersect b)


}

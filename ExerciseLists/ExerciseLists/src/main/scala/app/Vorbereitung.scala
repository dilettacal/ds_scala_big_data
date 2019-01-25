package app

import list.implementation.{Cons, Empty}
import list.traits.IntList

object Vorbereitung extends  App {


  def duplicates(l: IntList): Any = l match {
    case Empty => ""
    case Cons(x, Empty) => ""
    case Cons(x,xs) => {
      if(x == xs.head && x == xs.tail.head) xs.tail.head.toString + duplicates(xs.tail)
      else if(x == xs.head && x!= xs.tail.head) x.toString + duplicates(xs)
      else duplicates(xs)
    }
  }

  def flatten(ls: IntList): String = ls match {
    case Empty => ""
    case _ => {
      ls.head.toString + flatten(ls.tail)
    }
  }

  def flattenToList(ls: IntList): List[Int] = ls match {
    case Empty => List()
    case _ => ls.head::flattenToList(ls.tail)
  }

  def duplicates2(l: IntList) = {
    val flat = flattenToList(l)
    flat.groupBy(identity).mapValues(_.size).filter(x => x._2 >= 2)
  }
  //Result: 2211

  val list = Cons(4, Cons(2,Cons(2,Cons(2,Cons(2,Cons(2, Cons(3,Cons(11,Cons(11,Cons(3, Empty))))))))))
  println(flattenToList(list))
  println("Result: " + duplicates2(list))

  val dup = List(1,1,1,2,3,4,5,5,6,100,101,101,102)
  println(dup.groupBy(identity).collect{ case (x, List(_,_,_*)) => x.})

  val grades = List(
    ("Hans", "Datenbanken", 2.3),
    ("Maria", "Prog1", 1.0),
    ("Maria", "Prog3", 1.7),
    ("Hans", "Prog2", 1.7),
    ("Marco", "Prog1", 1.7),
    ("Maria", "Mathe1", 2.3),
    ("Marco", "Datenbanken", 3.3))

  val gradesbyStudent = grades.flatMap(x => grades map (y => (x,y)) groupBy(x => x._1))

  println(gradesbyStudent)



  println("'#####################")

  val db = List(("X", "acqua"), ("Y", "fuoco"), ("Z", "acqua"), ("X", "Terra"))

  println(db.map(x => (x._1, db.filter(y => x._1 == y._1) map (_._2))))

  val result = for (elem <- grades)
    yield (elem._1,
    for (tupel <- grades
      if(tupel._1.equals(elem._1))) yield (tupel._2, tupel._3) )

  val allGrades = grades.map(tupel => (tupel._1, grades.filter(x => x._1.equals(tupel._1)).map(data => (data._2, data._3))))

  /*
  List((Hans,List((Datenbanken,2.3), (Prog2,1.7))),
  (Maria,List((Prog1,1.0), (Prog3,1.7), (Mathe1,2.3))),
  (Maria,List((Prog1,1.0), (Prog3,1.7), (Mathe1,2.3))), (
  Hans,List((Datenbanken,2.3), (Prog2,1.7))),
  (Marco,List((Prog1,1.7), (Datenbanken,3.3))),
  (Maria,List((Prog1,1.0), (Prog3,1.7), (Mathe1,2.3))),
  ( Marco,List((Prog1,1.7), (Datenbanken,3.3))))
   */

  println(result)
  println(allGrades)

  val avgPerLecture = {
    grades.groupBy(x => x._2).map(x => (x._1, x._2.map(elem => elem._3).sum/x._2.size)).toMap
  }

  println(avgPerLecture)

  println(grades.groupBy(x => x._2).map(x => x._1))
  /*
  Map(Prog2 -> List((Hans,Prog2,1.7)), Mathe1 -> List((Maria,Mathe1,2.3)), Datenbanken -> List((Hans,Datenbanken,2.3), (Marco,Datenbanken,3.3)), Prog1 -> List((Maria,Prog1,1.0), (Marco,Prog1,1.7)), Prog3 -> List((Maria,Prog3,1.7)))
   */


 val vec = Vector(1,5,4,6)

  val l2 = math.sqrt(vec.foldLeft(0.0)((x,y) => x+(y*y)))
  println(l2)


  //MAP Reduce

  //Count the number of occurrences of prime divisors given a list of numbers


  //Average
/*
  val listToAvg = List(1,6,14,85,4)
  def avg(sum: Int, list: List[Int]): Double = sum/list.size
  listToAvg.reduceLeft((0) (sum, x) => sum+x/listToAvg.size)) */




}

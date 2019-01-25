object MapReduce {
  def mapper[KeyIn ,ValueIn, KeyMOut, ValueMOut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut,ValueMOut)],
                                                 data:List[(KeyIn, ValueIn)]):List[(KeyMOut,ValueMOut)]={

    data.flatMap(mapFun(_))
  }

  def sorter[KeyMOut,ValueMOut](data:List[(KeyMOut,ValueMOut)]):List[(KeyMOut,List[ValueMOut])]= {
    data.groupBy(_._1) mapValues(X=> X.map(_._2)) toList

  }

  def reducer[KeyMOut,ValueMOut,KeyROut, ValueROut](redFun:((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)],
                                                    data:List[(KeyMOut,List[ValueMOut])]):List[(KeyROut, ValueROut)]={

    data flatMap (redFun(_))
  }

  def mapReduce[KeyIn,ValueIn,KeyMOut,ValueMOut, KeyROut, ValueROut](mapFun:((KeyIn,ValueIn))=>List[(KeyMOut, ValueMOut)],
                                                                     redFun:(((KeyMOut,List[ValueMOut]))=>List[(KeyROut,ValueROut)]),
                                                                     data:List[(KeyIn,ValueIn)]): List[(KeyROut,ValueROut)] = {
    reducer(redFun,sorter(mapper(mapFun, data)))
  }

  def main(args: Array[String]): Unit = {
    wordCount()
  }

  def wordCount(): Unit ={
    val text = List((0,"Dies ist ein Test"),(1,"und jetzt kommt noch ein Test!"), (2,"mal schauen, ob es funktioniert"))
    println(mapReduce[Int, String, String, Int, String, Int](word=>word._2.split(" ").map(x=>(x, 1)).toList, x=>List((x._1, x._2.length)),text))
  }

  //List((Dies,1), (und,1), (ist,1), (funktioniert,1), (Test!,1), (ein,2), (mal,1), (noch,1), (ob,1), (es,1), (Test,1), (kommt,1), (jetzt,1), (schauen,,1))
}
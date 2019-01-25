object forExercises {
  def main(args: Array[String]): Unit = {
    getAllPossibleCombinations()

    println(getFilmsSeenByFrancescoGenerators())
    println(getFilmsSeenByFrancescoMapFilter())

    println(getPeopleWhoHaveSeenJamesBondGenerator())
    println(getPeopleWhoHaveSeenJamesBondMapFilter())

    println(getPeopeWhoHaveSeenMoreThanTwoFilmsGenerators())
    println(getPeopeWhoHaveSeenMoreThanTwoFilmsMapFilter())

    println(getListWithAllFilmsSomeoneHasSeenGenerator())
    println(getListWithAllFilmsSomeoneHasSeenMapFilter())

    /*
    (A,a), (B,a), (C,a), (D,a), (A,b), (B,b), (C,b), (D,b), (A,c), (B,c), (C,c), (D,c), (A,d), (B,d), (C,d), (D,d)
    List(bloodsports, die12KammernDerShaolin)
    List(bloodsports, die12KammernDerShaolin)
    List(simon, marcus)
    List(simon, marcus)
    List(francesco, francesco)
    List(francesco, francesco)
    List((francesco,List(bloodsports, die12KammernDerShaolin)), (simon,List(jamesBond)), (marcus,List(jamesBond)), (francesco,List(bloodsports, die12KammernDerShaolin)))
    List((francesco,List(bloodsports, die12KammernDerShaolin)), (simon,List(jamesBond)), (marcus,List(jamesBond)), (francesco,List(bloodsports, die12KammernDerShaolin)))

     */
  }

  def getFilmsSeenByFrancescoGenerators(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    for (entry <- db if entry._1.equals("francesco")) yield entry._2
  }

  def getFilmsSeenByFrancescoMapFilter(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    db.filter(entry => entry._1.equals("francesco")).map(entry => entry._2)
  }

  def getPeopleWhoHaveSeenJamesBondGenerator(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    for (entry <- db if entry._2.equals("jamesBond")) yield entry._1
  }

  def getPeopleWhoHaveSeenJamesBondMapFilter(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    db.filter(entry => entry._2.equals("jamesBond")).map(entry => entry._1)
  }

  def getPeopeWhoHaveSeenMoreThanTwoFilmsGenerators(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    for (entry1 <- db; entry2 <- db if entry1._1.equals(entry2._1) && !entry1._2.equals(entry2._2)) yield entry1._1
  }

  def getPeopeWhoHaveSeenMoreThanTwoFilmsMapFilter(): List[String] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    db.flatMap(entry1 => db.filter(entry2 => entry1._1.equals(entry2._1) && !entry1._2.equals(entry2._2))).map(x => x._1)
  }

  def getListWithAllFilmsSomeoneHasSeenGenerator(): List[(String, List[String])] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    val list = for (x <- db) yield (x._1, for (y <- db if y._1.equals(x._1)) yield y._2)
    list
  }

  def getListWithAllFilmsSomeoneHasSeenMapFilter(): List[(String, List[String])] = {
    val db = List(("francesco", "bloodsports"), ("simon", "jamesBond"), ("marcus", "jamesBond"), ("francesco", "die12KammernDerShaolin"))
    db.map(x => (x._1, db.filter(y => y._1.equals(x._1)).map(y => y._2)))
  }


  def getAllPossibleCombinations(): Unit = {
    val list1 = List("a", "b", "c", "d")
    val list2 = List("A", "B", "C", "D")

    val allCombinations: List[(String, String)] = for (elem1 <- list1; elem2 <- list2) yield (elem2, elem1)
    println(allCombinations.mkString(", "))
  }
}
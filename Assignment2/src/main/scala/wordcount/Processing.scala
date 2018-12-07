package wordcount

class Processing {
   
  /**********************************************************************************************
   *
   *                          Aufgabe 1
   *   
   *********************************************************************************************
  */
  def getWords(line:String):List[String]={
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    val regex = "[^a-zA-Z]"
    if(line == "")
      return List()
    else
      line.replaceAll(regex, " ").toLowerCase.split(" ").filter(x => x != "").toList
  } 
  
  def getAllWords(l:List[(Int,String)]):List[String]={
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     *
     * List of tuples:
     * List((0,CHAPTER I—START IN LIFE), (1,), (2,I was born in the year 1632, in the city of York,...))
     * - Goal: Extracat words from the second content
     * tuple = (int, string) --> tuple._2
     */
      l.flatMap(tuple => getWords(tuple._2))
  }

  def countWords(l:List[String]):List[(String,Int)]={
    /*
     *  Gets a list of words and counts the occurrences
     *  of the individual words
     *
     *  Repepetitions should be considered as 1 word
     */
    l
      .flatMap(getWords) //Wörter werden aus Liste gezogen
      .groupBy(word => word) //Woerter werden hier gruppiert: z.B.: (lion,List(lion, lion, lion, lion))
      .map(tuple =>
        (tuple._1, tuple._2.size)) // (lion,4)
      .toList //Rueckgabe als Liste

  }

  /**********************************************************************************************
   *
   *                          Aufgabe 2
   *
   *********************************************************************************************
  */

  def mapReduce[S,B,R](mapFun:(S=>B),
      redFun:(R,B)=>R,
      base:R,
      l:List[S]):R =

  l.map(mapFun).foldLeft(base)(redFun)

  def countWordsMR(l: List[String]): List[(String, Int)] = {
  //mapReduce[???,???,???](null,null,null,l)
    //Ergebnis aus MapFunction, Zwischenergebnis, Base
    mapReduce[String, (String, Int), Map[String, Int]](
      word => (word, 1), //map
      (wordToCounter, tupleWithWordAndCount)
        => {
        println("(Counter, Tuple)\n (" + wordToCounter + ", " + tupleWithWordAndCount + ")")
        wordToCounter.updated(tupleWithWordAndCount._1, 1 + wordToCounter.getOrElse(tupleWithWordAndCount._1, 0))
      },
      Map[String,Int](),
      l
    ).toList
  }
  
  
  /**********************************************************************************************
   *
   *                          Aufgabe 3
   *   
   *********************************************************************************************
  */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    ???
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    ???
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    ???
  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    ???
  }
}


object Processing{

  def getData(filename:String):List[(Int,String)]={
    val url= getClass.getResource("/"+filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    var c = -1
    val result= (for (row <- iter) yield {c=c+1;(c,row)}).toList
    src.close()
    result
  }

}
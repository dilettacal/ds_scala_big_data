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

  /**
    * Die methode extrahiert alle Wörter aus dem Parameter znd liefert ergebnis.
    * @param l List in der Form (Zeilennr., Zeilentext)
    * @return Liste aus Tupeln mit (Zeilennr und Wort)
    */
  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l
      .flatMap(rowNr_text =>
        getWords(rowNr_text._2)
       //Tokenization + Stopwords entfernen
      .map(
        (rowNr_text._1, _)
      )) //Mapping der Wörter auf die Indizes - welcher Tupel gehört das Wort?

    //Beispielergebnis: List((0,this), (0,is), (0,a), (0,test), (0,the), (0,result),...)
  }

  /**
    * List: (zeilennr, word) - s.o.
    * InverseIndex: word --> line_nr
    * beispiel:
    *   Hello -> [1,2,13]
    *   Für jedes Wort sollen die Zeilen ermittelt werden, in denen das Wort vorkommt und sie als Liste von Wort,[Zeilen] zurückgegeben werden
    * @param l
    * @return
    */
  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
      l.foldLeft(Map[String, List[Int]]())((list, word_linenr) =>{
        list.updated(word_linenr._2, word_linenr._1 :: list.getOrElse(word_linenr._2, List()))
    }).mapValues(l => l.reverse) //reverse notwendig, sonst: (this, (2,0)) statt (this, (0,2))
  }

  /**
    * Liefert die Zeilen zurück, in denen mindenstens ein Wort aus der Liste enthalten ist
    * @param words
    * @param invInd
    * @return
    */

  /*
  Beispiel:
  1. Param 1: List of words: List(hello, test)
  2. Param 2: List of invInd: Map(test -> List(0, 2), this -> List(0, 2), in -> List(2), are -> List(2), is -> List(0, 2), another -> List(2), result -> List(0), it -> List(2), a -> List(0, 2), string -> List(2), also -> List(2), should -> List(0), lot -> List(2), words -> List(0, 2), which -> List(2), be -> List(0), contains -> List(2), of -> List(2), the -> List(0))
  3. Nur test ist in Zeilen 0,2 enthalten
  3.1 Rückgabe soll List(0,2) sein
   */
  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    //TODO Change this solution to a better solution!

    //Untersuche Liste words
    words.map(word => { //mapp die Wörter aus words
      if (invInd.contains(word)){ //entweder auf die entsprechende Liste (wenn word in words in invInd überhaupt drinnen ist)
        invInd(word)
      }
      else List() //oder auf die leere Liste, falls word in invInd nicht enthalten ist
      //list1 == List(), list2 == List(0,2)
    }).reduceLeft((list1, list2) => { //Jetzt haben wir: List(List(), List(0,2)) --> List() UNION List(0,2) --> List(0,2)
      //List() INTERSECT List(0,2) -> List()
      list1.union(list2)
    })
  }


  /**
    * Liefert die Zeilen zurück, die alle Wörter enthalten
    * @param words
    * @param invInd
    * @return
    */
  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    words.map(word =>
      if(invInd.contains(word)) invInd(word)
      else List()
    ).reduceLeft((list1, list2) => list1.intersect(list2))
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
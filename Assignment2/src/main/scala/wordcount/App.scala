package wordcount

import scala.collection.JavaConverters._

/**
  * @author hendrik
  * modified by akarakochev
  */
object App {

  val sentiAnalyse = new Sentiments("AFINN-112.txt")
  val processing = new Processing()

  /*
   * 
   *   Diese Anwendung fuehrt eine Sentiment-Analyse mit
   *   dem Buch "Robinson Crusoe" von Daniel Defoe (1660-1731) durch
   *
   *   Es werden zwei Graphen erstellt. Der eine beinhaltet die Stimmung
   *   in den ueber 10000 Woertern erzeugten Absaetzen.
   *   Der zweite Graph enthaelt die relative Haeufigkeit der erkannten Woerter,
   *   d.h. die Anzahl der Woerter, die für die Analyse der Stimmung herangezogen wurden.
   *
   *  Testen Sie die App mit verschiedenen Absatzgroessen
   *
   *
   *  Kommentieren sie die Dritte Zeile der Main Methode raus.
   *
   *  So werden wieder zwei Graphen erstellt.
   *  Der erste beinhaltet die Stimmung ueber die Kapitel.
   *  Wie davor enthaelt der zweite Graph die relative Haeufigkeit der erkannten Woerter,
   *  d.h. die Anzahl der Woerter, die für die Analyse der Stimmung herangezogen wurden.
   *  Besonders interessant sind Kapitel 7 (CHAPTER VII—AGRICULTURAL EXPERIENCE ),
   *  was eine eher positive Stimmung haben sollte und z.b
   *  6 (CHAPTER VI—ILL AND CONSCIENCE-STRICKEN) und
   *  12(CHAPTER XII—A CAVE RETREAT), die eher negativ sind.
   */
  def main(args: Array[String]) = {
    val fileName = "Robinson.txt"
    showAnalysisPerSegment(10000, fileName)
    showAnalysisPerChapter(fileName)
    val data = Processing.getData(fileName)
    println(data)
    val string = "\nI was born in the year 1632, in the city of York, of a good family,\nthough not of that country, my father being a foreigner of Bremen, who\nsettled first at Hull.  He got a good estate by merchandise, and leaving\noff his trade, lived afterwards at York, from whence he had married my\nmother, whose relations were named Robinson, a very good family in that\ncountry, and from whom I was called Robinson Kreutznaer; but, by the\nusual corruption of words in England, we are now called—nay we call\nourselves and write our name—Crusoe; and so my companions always called\nme.\n\nI had two elder brothers, one of whom was lieutenant-colonel to an\nEnglish regiment of foot in Flanders, formerly commanded by the famous\nColonel Lockhart, and was killed at the battle near Dunkirk against the\nSpaniards.  What became of my second brother I never knew, any more than\nmy father or mother knew what became of me.\n\nBeing the third son of the family and not bred to any trade, my head\nbegan to be filled very early with rambling thoughts.  My father, who was\nvery ancient, had given me a competent share of learning, as far as\nhouse-education and a country free school generally go, and designed me\nfor the law; but I would be satisfied with nothing but going to sea; and\nmy inclination to this led me so strongly against the will, nay, the\ncommands of my father, and against all the entreaties and persuasions of\nmy mother and other friends, that there seemed to be something fatal in\nthat propensity of nature, tending directly to the life of misery which\nwas to befall me.\n\nMy father, a wise and grave man, gave me serious and excellent counsel\nagainst what he foresaw was my design.  He called me one morning into his\nchamber, where he was confined by the gout, and expostulated very warmly\nwith me upon this subject.  He asked me what reasons, more than a mere\nwandering inclination, I had for leaving father’s house and my native\ncountry, where I might be well introduced, and had a prospect of raising\nmy fortune by application and industry, with a life of ease and pleasure.\nHe told me it was men of desperate fortunes on one hand, or of aspiring,\nsuperior fortunes on the other, who went abroad upon adventures, to rise\nby enterprise, and make themselves famous in undertakings of a nature out\nof the common road; that these things were all either too far above me or\ntoo far below me; that mine was the middle state, or what might be called\nthe upper station of low life, which he had found, by long experience,\nwas the best state in the world, the most suited to human happiness, not\nexposed to the miseries and hardships, the labour and sufferings of the\nmechanic part of mankind, and not embarrassed with the pride, luxury,\nambition, and envy of the upper part of mankind.  He told me I might\njudge of the happiness of this state by this one thing—viz. that this was\nthe state of life which all other people envied; that kings have\nfrequently lamented the miserable consequence of being born to great\nthings, and wished they had been placed in the middle of the two\nextremes, between the mean and the great; that the wise man gave his\ntestimony to this, as the standard of felicity, when he prayed to have\nneither poverty nor riches."
    println(processing.getWords(string))

    val allWords = processing.getAllWords(data)
    println(processing.countWords(allWords))
    //println(processing.countWordsMR(allWords))
    val l1 = List()
    val l2 = List(1, 2)
    println(l1.union(l2))


  }

  def showAnalysisPerSegment(n: Int, fileName: String): Unit = {
    val book = sentiAnalyse.getDocumentGroupedByCounts(fileName, n)
    val data = sentiAnalyse.analyseSentiments(book)
    sentiAnalyse.createGraph(data, title = s"Sentiment-Analyse: Abschnitte je $n Wörter")
  }

  def showAnalysisPerChapter(fileName: String): Unit = {
    val chapterRegex = "CHAPTER [MDCLXVI]+".r

    val book = sentiAnalyse.getDocumentSplitByPredicate(fileName,
      x => chapterRegex.findFirstIn(x).isDefined)
    val data = sentiAnalyse.analyseSentiments(book)
    sentiAnalyse.createGraph(data, "Kapitel", "Sentiment-Analyse: Kapitel")
  }
}
package wordcount

import java.awt.{Color, GridLayout}

import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities


/**
  * @author hendrik
  * modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 5
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {
    val processedFile = Processing.getData(filename) //map
    println("FILE: " + processedFile)
    //List((0,Call me Ishmael. Some years ago--never mind how long precisely--having), (1,little or no money in my purse, and nothing particular to interest me on), ....
    processedFile.flatMap( //Aus der Mappe nur Wörter extrahieren
      elem =>
      proc.getWords(elem._2)) //0, (call, me, Ishmael, some, ..)
      .grouped(wordCount) //Wörter nach wordCount gruppieren
      .zipWithIndex //der neuen Liste wird ein Index angehangen
      .map{
          x => {
            println("X: " + x) //wordcount=5 --> (List(call, me, ishmael, some, years),0)
            (x._2+1, x._1) //(List(1, (call, me, ishmael, some, years))
          }
      }.toList

  }

  def getDocumentSplitByPredicate(filename: String, predicate:String=>Boolean): List[(Int, List[String])] = ???
  /*{
    val processedFile = Processing.getData(filename) //map
    processedFile.flatMap( //Aus der Mappe nur Wörter extrahieren
      elem =>
        proc.getWords(elem._2))
        .partition(x => predicate(x))
      return List(1,List("a"))
  }
*/
  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {

    //Erzeugt eine Liste, die Abschnitt, Sentimentwörter und gesamte Anzahl der Wörter enthält
    val sentiWordsAndValues = l.foldLeft(List[(Int, List[(String, Int)], Int)]())((list, paragraph) => {
      list++List((paragraph._1, paragraph._2.foldLeft(List[(String, Int)]())((list, word) =>
        sentiments.get(word) match{
        //Wort wird hingefügt, wenn es ein Sentimentwort ist
        case Some(elem) => List.concat(list, List((word, sentiments.get(word).last))) //NoSuchElementException
        //sonst nicht
        case None => list
      }), paragraph._2.length))

    })

   //Dieser Teil liefert korrektes Format zurück

    val result = sentiWordsAndValues.foldLeft(List[(Int, Double, Double)]())((list, paragraph) => {
      val sentiment_values = paragraph._2.foldLeft(List[Int]())((values, word) => word._2 :: values).sum.toDouble / paragraph._2.length.toDouble
      val rel_words_used = paragraph._2.length.toDouble / paragraph._3.toDouble
      List.concat(list, List((paragraph._1, sentiment_values, rel_words_used)))
    })
    result
  }

  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url)
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel:String="Abschnitt", title:String="Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2,1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
  }
}

package twitter

import org.apache.spark.rdd.RDD
import twitter.models.Tweet

/**
  * @param tData twitter data rdd
  */
class TwitterAnalyzer(tData: RDD[Tweet]) {

  /*
   * Write a function that counts the number of tweets using the german language
   */
  def getGermanTweetsCount: Long = {
    //eq not working
    tData.filter(_.lang.equals("de")).count()
  }

  /*
   * Extracts the texts of all german tweets (all tweets with the "de" locale)
   */
  def getGermanTweetTexts: Array[String] = {
    tData.filter(_.lang.equals("de")).map(tweet => tweet.text).collect
  }

  /**
    * Counts the number of unique german users (all users that tweeted using the "de" locale)
    */
  def numberOfGermanTweetsPerUser: Array[(String, Int)] = {
    tData
      .filter(_.lang.equals("de"))
      .groupBy(_.userName)
      .mapValues(_.toList.length)
      .collect()
  }

  /**
    * Counts the number of tweets per country
    */
  def numberOfTweetsPerCountry: Array[(String, Int)] = {
    tData.groupBy(tweet => tweet.lang).mapValues(_.toList.length).collect
  }

  /**
    * Extracts the top 10 hashtags in english tweets (tweets with the "en" locale).
    *
    * Hints:
    * Use the [[TwitterAnalyzer.getHashtags()]] function to obtain the hashtags from the text of a tweet.
    * Checkout RDD.takeOrdered
    * https://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.rdd.RDD@takeOrdered(num:Int)(implicitord:Ordering[T]):Array[T]
    * or RDD.top
    * https://spark.apache.org/docs/latest/api/scala/index.html#org.apache.spark.rdd.RDD@top(num:Int)(implicitord:Ordering[T]):Array[T]
    * and Ordering.by
    */
  def getTopTenEnglishHashtags: List[(String, Int)] = {
    //TODO: Kürzer machen?
    /*
    Tweet Text: RT @SammyTellem: sometimes we should be really really grateful...😥💔🙏 http://t.co/oWa9gWsN37
    FlatMap L: List(#pics, #news, #pussy, #youtube)
     */
    tData
      .filter(tweet => tweet.lang.equals("en")) //Alle en-Tweets
      .map(tweet => tweet.text) //alle en-Tweet-Texte
      .map(TwitterAnalyzer.getHashtags) //Alle Hashtags
      .filter(res => !res.isEmpty) //vermeidet List() im nächsten Schritt
      .flatMap(hashtag => hashtag) //List(#pics, #news, #pussy, #youtube) - erzeugt eine Liste von Hashtags für jeden Tweet
      .groupBy(hashtagValue => hashtagValue) //gruppieren nach hashtags
      //Hash: CompactBuffer(#Badgers, #Badgers, #Badgers, #Badgers, #Badgers)
      //List("Badgers", 5)
      .mapValues(hash => hash.toList.length) //aufzaehlen der hastags für jede Gruppe
      //die 10 besten Hashtags werden zurückgegeben
      //nach deren Anzahl (Stelle 2)
      .takeOrdered(10)(Ordering[Int].reverse.on(sl => sl._2))
      .toList
  }

}

object TwitterAnalyzer {

  def getHashtags(text: String): List[String] = {
    if (text.isEmpty || text.length == 1) List()
    else if (text.head == '#') {
      val tag = text.takeWhile(x => x != ' ')
      val rest = text.drop(tag.length)
      tag :: getHashtags(rest)
    }
    else getHashtags(text.tail)
  }
}
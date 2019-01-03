package twitter

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import twitter.models.Tweet
import utils.JsonUtils

object TwitterUtilities {
  val dtf: DateTimeFormatter = DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss X uuuu", Locale.ENGLISH)

  /**
    * Parses a given tweet in the Twitter Data JSON Format using [[JsonUtils.parseJson()]]
    * and extracts the date, username, text and language of the tweet into a [[Tweet]] object.
    * The [[TwitterUtilities.getTwitterDate()]] function is used to parse the date.
    * If the line is not a valid json string, None is returned instead.
    *
    * Hints:
    * Analyse the Twitter Data Format (an entry is pasted above for convenience).
    * The date corresponds to the "created_at" field, the userName to the "name" field
    * and the text and language to the "text" and "lang" fields of the json string.
    *
    * Pay attention, that not all lines are actual tweets and don't comply to the above format.
    * (Checkout the tweets.txt File in the test resources)
    * Handle this by returning None
    *
    * If you are not too familiar with scala's Option class, checkout
    * https://www.scala-lang.org/api/2.12.x/scala/Option.html
    * and
    * https://www.tutorialspoint.com/scala/scala_options.htm
    *
    * If you are having trouble with casting checkout
    * https://alvinalexander.com/scala/how-to-cast-objects-class-instance-in-scala-asinstanceof
    */
  def parse(jsonString: String): Option[Tweet] = {

    val tweet = JsonUtils.parseJson(jsonString)
    tweet match {
        //Tweet(date: OffsetDateTime, userName: String, text: String, lang: String)
      case Some(map: Map[String, Any]) =>
        if (isValidTweetJSON(map))
          //Option class when returning a value from a function that can be null.
          Option(Tweet(
            getTwitterDate(map("created_at").toString),
            //Here retrieves all user block
            map("user").asInstanceOf[Map[String, Any]]("name").toString,
            map("text").toString,
            map("lang").toString
          ))
        else None
      case None => None
    }
  }

  // //Tweet(date: OffsetDateTime, userName: String, text: String, lang: String)
  private def isValidTweetJSON(map: Map[String, Any]): Boolean =
    (map.contains("created_at") && map.contains("user") && map.contains("text") && map.contains("lang"))

  def getTwitterDate(date: String): OffsetDateTime = {
    try {
      OffsetDateTime.parse(date, dtf)
    } catch {
      case e: Exception =>
        println(date)
        OffsetDateTime.now
    }
  }
}
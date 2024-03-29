import org.apache.spark.SparkContext
import org.apache.spark.SparkConf

/**
  * Created by toddmcgrath on 6/15/16.
  */
object SimpleSparkExample {
  def main(args: Array[String]) {
    val logFile = "src/main/resources/logme.md" // Should be some file on your system
    val conf = new SparkConf().setAppName("Simple Application").setMaster("local[*]")
    val sc = new SparkContext(conf)
    val logData = sc.textFile(logFile, 2).cache()
    val numAs = logData.filter(line => line.contains("a")).count()
    val numBs = logData.filter(line => line.contains("b")).count()
    println("Lines with a: %s, Lines with b: %s".format(numAs, numBs))
  }

}

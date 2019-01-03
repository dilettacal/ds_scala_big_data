package introduction

import org.apache.spark.SparkContext

/**
  * In this section you will practice some basic operations on RDDs.
  *
  * NOTE: complete Section0 before starting this.
  *
  */
object Section1 {
  def main(args: Array[String]): Unit = {

    Section0.initSparkContext(Section0.getClass.getName)

    //obtain a reference to the initialized spark context
    val sc = SparkContext.getOrCreate()
    println("Some information about the given context:")
    println(sc.sparkUser)
    println(sc.startTime)

    //create the data
    val types = List("a", "b", "c", "d")
    val divisors = List(2, 3, 5, 7)
    //println(types.zip(divisors))
    val data = for {(t, d) <- types.zip(divisors); x <- 1 to 100000; if x % d == 0} yield (t, x)
  //(a,43276), (a,43278), (a,43280), (a,43282), (a,43284),...

    //Resilient Distributed Dataset:
    val rdd = sc.parallelize(data).cache()

    /**
      * Count all entries of type "a"
      *
      * Hint: use filter and count
      */
    val entriesACount = rdd.filter(data => data._1.equals("a")).count()
    println(s"c(A) = $entriesACount")

    /**
      * Sum all entries of type "c"
      *
      * Hint:use filter, map and sum
      */
    val entriesBsum = rdd.filter(data => data._1.equals("b")).map(data => data._2).sum()
    println(s"\u2211(B) = $entriesBsum")

    /**
      * SEE: https://backtobazics.com/big-data/spark/apache-spark-reducebykey-example/
      * Sum all entries of each type.
      *
      * Hint: use reduceByKey followed by collect
      *
      * - Do all results look plausible? Actually not:
      * Entries to Sums: List((d,714264285), (a,-1794917296), (b,1666683333), (c,1000050000))
      *
      * - What happens if you omit collect? Ohne collect liefern wir noch ein RDD zurück
      */
      //reduceByKey(_ + _)
    val entriesToSums:Array[(String,Int)] = rdd.reduceByKey((accum, n) => (accum + n)).collect()
    println(s"Entries to Sums: ${entriesToSums.toList}")

    /**
      * Count all entries of each type
      * Use map, reduceByKey and collect
      *
      * Hint:In the map operation, map the values to identity (map the keys in order to return String, Int
      *
      * Result: List((d,14285), (a,50000), (b,33333), (c,20000))
      */
    val entriesToCounts:Array[(String,Int)] = rdd.map(data => (data._1, 1)).reduceByKey((accum, n) => (accum + n)).collect()
    println(s"Entries to Counts: ${entriesToCounts.toList}")

    /**
      * Count all entries of each type
      * Use mapValues, reduceByKey and collect
      *
      * Hint:In the mapValues operation, map to identity
      *
      * Result: List((d,14285), (a,50000), (b,33333), (c,20000))
      */
    val entriesToCounts2:Array[(String,Int)] = rdd.mapValues(_ => 1).reduceByKey((accum, n) => (accum + n)).collect()
    println(s"Entries to Counts MapValues: ${entriesToCounts2.toList}")


    /**
      * Count all entries of each type
      * Use the built in countByKey
      *
      * Result is a Map:
      * Map(d -> 14285, a -> 50000, b -> 33333, c -> 20000)
      */
    val entriesToCounts3 = rdd.countByKey()
    println(s"Entries to Counts CountByKey: ${entriesToCounts3}")

    //Checkout the internal implementation of countByKey for PairRDDs
    //https://github.com/apache/spark/blob/9b1f6c8bab5401258c653d4e2efb50e97c6d282f/core/src/main/scala/org/apache/spark/rdd/PairRDDFunctions.scala#L370

    //Checkout what pairRDDs are
    //https://spark.apache.org/docs/latest/rdd-programming-guide.html#working-with-key-value-pairs

    Section0.tearDownSparkContext()
  }
}